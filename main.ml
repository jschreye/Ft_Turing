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
(* 2) Chargement, validation de l’entrée et simulation                          *)
(* -------------------------------------------------------------------------- *)

let load_machine json_file input =
  (* 2.1) Lecture du JSON *)
  let json = Yojson.Basic.from_file json_file in

  (* 2.2) Extraction de l’alphabet et du blanc pour valider l’entrée *)
  let alphabet = json |> member "alphabet" |> to_list |> filter_string in
  let blank    = json |> member "blank"    |> to_string in

  (* 2.3) Validation que chaque caractère d’input est bien dans l’alphabet *)
  String.iter (fun c ->
    let s = String.make 1 c in
    if s = blank then (
      Printf.eprintf "❌ Erreur : le symbole blanc '%s' ne peut pas être dans l’entrée\n" blank;
      exit 1
    ) else if not (List.mem s alphabet) then (
      Printf.eprintf "❌ Erreur : symbole non reconnu '%s' dans l’entrée\n" s;
      exit 1
    )
  ) input;

  (* 2.4) Extraction des autres champs *)
  let name     = json |> member "name"     |> to_string in
  let initial  = json |> member "initial"  |> to_string in
  let finals   = json |> member "finals"   |> to_list |> filter_string in
  let trs      = parse_transitions json in
  let states   = json |> member "states"   |> to_list |> filter_string in

  (* 2.6) Validation formelle de la machine *)
  validate_machine
    ~alphabet
    ~states
    ~initial
    ~finals
    ~blank
    ~transitions:trs;

  (* 2.7) Affichage des infos et lancement de la simulation *)
  Printf.printf "Nom de la machine : %s\n" name;
  Printf.printf "Entrée reçue       : %s\n\n" input;
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
