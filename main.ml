(* main.ml *)

open Turinglib.Tape
open Turinglib.Parser
open Turinglib.Simulator
open Yojson.Basic.Util

(* -------------------------------------------------------------------------- *)
(* 1) Affichage de l’aide                                                      *)
(* -------------------------------------------------------------------------- *)

let usage = "usage: ft_turing [-h] jsonfile input"

let print_help () =
  Printf.printf "%s\n\n" usage;
  Printf.printf "positional arguments:\n";
  Printf.printf "  jsonfile   json description of the machine\n";
  Printf.printf "  input      input of the machine\n\n";
  Printf.printf "optional arguments:\n";
  Printf.printf "  -h, --help show this help message and exit\n";
  exit 0

(* -------------------------------------------------------------------------- *)
(* 2) Chargement et simulation de la machine                                   *)
(* -------------------------------------------------------------------------- *)

let load_machine json_file input =
  (* Lecture du JSON *)
  let json = Yojson.Basic.from_file json_file in
  let name = json |> member "name" |> to_string in

  (* Underflow check pour unary_sub *)
  if name = "unary_sub" then begin
    (try
       let dash = String.index input '-' in
       let eq   = String.index input '=' in
       let left  = String.sub input 0 dash in
       let right = String.sub input (dash + 1) (eq - dash - 1) in
       let count_ones s =
         String.fold_left (fun acc c -> if c = '1' then acc + 1 else acc) 0 s
       in
       let nl = count_ones left and nr = count_ones right in
       if nl < nr then (
         Printf.eprintf
           "❌ Erreur : soustraction impossible %d - %d (underflow)\n"
           nl nr;
         exit 1
       )
     with Not_found -> ())
  end;

  (* Extraction des champs du JSON *)
  let alphabet = json |> member "alphabet" |> to_list |> filter_string in
  let initial  = json |> member "initial"  |> to_string in
  let finals   = json |> member "finals"   |> to_list |> filter_string in
  let blank    = json |> member "blank"    |> to_string in
  let trs      = parse_transitions json in
  let states   = json |> member "states"   |> to_list |> filter_string in

  (* Vérification formelle *)
  validate_machine ~alphabet ~states ~initial ~finals ~blank ~transitions:trs;

  (* Affichage des infos *)
  Printf.printf "Nom de la machine : %s\n" name;
  Printf.printf "Entrée reçue       : %s\n\n" input;

  (* Initialisation du ruban et lancement de la simulation *)
  let tape0 = init_tape input blank in
  run ~blank ~trs ~finals ~state:initial ~tape:tape0 ~steps_left:1000 1

(* -------------------------------------------------------------------------- *)
(* 3) Point d’entrée : parsing des options et des arguments                   *)
(* -------------------------------------------------------------------------- *)

let () =
  let specs = [
    ("-h", Arg.Unit print_help, " Affiche ce message d’aide");
    ("--help", Arg.Unit print_help, " Même chose que -h");
  ] in
  let anon = ref [] in
  Arg.parse specs (fun s -> anon := s :: !anon) usage;
  match List.rev !anon with
  | [json_file; input] ->
      load_machine json_file input
  | _ ->
      print_help ()
