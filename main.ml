open Turinglib.Tape
open Turinglib.Parser
open Turinglib.Simulator
open Yojson.Basic.Util

(* -------------------------------------------------------------------------- *)
(* 1) Affichage de l’aide                                                      *)
(* -------------------------------------------------------------------------- *)

let usage = "usage: ft_turing [--alphabet ...] [--blank ...] [--states ...] jsonfile input"

let print_help () =
  Printf.printf "%s\n\n" usage;
  Printf.printf "positional arguments:\n";
  Printf.printf "  jsonfile   json description of the machine\n";
  Printf.printf "  input      input of the machine\n\n";
  Printf.printf "optional arguments:\n";
  Printf.printf "  -h, --help      show this help message and exit\n";
  Printf.printf "  --alphabet STR alphabet (e.g. \"1+=.\") if missing from JSON\n";
  Printf.printf "  --blank STR    blank symbol (e.g. \".\") if missing from JSON\n";
  Printf.printf "  --states STR   comma-separated list of states if missing from JSON\n";
  exit 0

(* -------------------------------------------------------------------------- *)
(* 2) Chargement, validation de l’entrée et simulation                         *)
(* -------------------------------------------------------------------------- *)

let load_machine json_file input alphabet_arg blank_arg states_arg =
  let json = Yojson.Basic.from_file json_file in

  (* Fallback: alphabet depuis JSON ou argument *)
  let alphabet =
    try json |> member "alphabet" |> to_list |> filter_string
    with _ ->
      if !alphabet_arg = "" then (
        Printf.eprintf "❌ Erreur : alphabet manquant dans le JSON et aucun argument --alphabet fourni\n";
        exit 1
      );
      let chars = ref [] in
      String.iter (fun c -> chars := (String.make 1 c) :: !chars) !alphabet_arg;
      List.rev !chars
  in

  let blank =
    try json |> member "blank" |> to_string
    with _ ->
      if !blank_arg = "" then (
        Printf.eprintf "❌ Erreur : blank manquant dans le JSON et aucun argument --blank fourni\n";
        exit 1
      );
      !blank_arg
  in

  let states =
    try json |> member "states" |> to_list |> filter_string
    with _ ->
      if !states_arg = "" then (
        Printf.eprintf "❌ Erreur : états manquants dans le JSON et aucun argument --states fourni\n";
        exit 1
      );
      String.split_on_char ',' !states_arg |> List.map String.trim
  in

  (* Validation de l’entrée *)
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

  let name    = json |> member "name"    |> to_string in
  let initial = json |> member "initial" |> to_string in
  let finals  = json |> member "finals"  |> to_list |> filter_string in
  let trs     = parse_transitions json in

  validate_machine
    ~alphabet
    ~states
    ~initial
    ~finals
    ~blank
    ~transitions:trs;

  Printf.printf "Nom de la machine : %s\n" name;
  Printf.printf "Entrée reçue       : %s\n\n" input;
  let tape0 = init_tape input blank in
  run ~blank ~trs ~finals ~state:initial ~tape:tape0 ~steps_left:1000 1

(* -------------------------------------------------------------------------- *)
(* 3) Point d’entrée                                                           *)
(* -------------------------------------------------------------------------- *)

let () =
  let alphabet_arg = ref "" in
  let blank_arg = ref "" in
  let states_arg = ref "" in
  let anon = ref [] in

  let specs = [
    ("-h", Arg.Unit print_help, " Affiche ce message d’aide");
    ("--help", Arg.Unit print_help, " Même chose que -h");
    ("--alphabet", Arg.Set_string alphabet_arg, "Alphabet (ex: \"1+=.\")");
    ("--blank", Arg.Set_string blank_arg, "Symbole blanc (ex: \".\")");
    ("--states", Arg.Set_string states_arg, "États séparés par des virgules");
  ] in

  Arg.parse specs (fun s -> anon := s :: !anon) usage;

  match List.rev !anon with
  | [json_file; input] ->
      load_machine json_file input alphabet_arg blank_arg states_arg
  | _ ->
      print_help ()
