
open Turinglib.Tape
open Turinglib.Parser
open Turinglib.Simulator
open Yojson.Basic.Util

let print_help () =
  print_endline "usage: ft_turing [-h] jsonfile input";
  print_endline "\npositional arguments:";
  print_endline "  jsonfile   json description of the machine";
  print_endline "  input      input of the machine";
  print_endline "\noptional arguments:";
  print_endline "  -h, --help show this help message and exit";
  exit 0

let load_machine json_file input =
  let json     = Yojson.Basic.from_file json_file in
  let name     = json |> member "name"     |> to_string in
  let alphabet = json |> member "alphabet" |> to_list |> filter_string in
  let initial  = json |> member "initial"  |> to_string in
  let finals   = json |> member "finals"   |> to_list |> filter_string in
  let blank    = json |> member "blank"    |> to_string in
  let trs      = parse_transitions json in
  let states   = json |> member "states"   |> to_list |> filter_string in

  (* ✅ Vérification complète *)
  validate_machine ~alphabet ~states ~initial ~finals ~blank ~transitions:trs;

  Printf.printf "Nom de la machine : %s\n" name;
  Printf.printf "Alphabet : [ %s ]\n" (String.concat ", " alphabet);
  Printf.printf "État initial : %s\n" initial;
  Printf.printf "États finaux : [ %s ]\n\n" (String.concat ", " finals);

  let tape0 = init_tape input blank in
  Printf.printf "État initial de la bande :\n";
  show_tape tape0;
  
  Printf.printf "\n🔁 Démarrage de la simulation...\n\n";
  run ~blank ~trs ~finals ~state:initial ~tape:tape0 ~steps_left:1000

let () =
  if Array.length Sys.argv < 2 then print_help ();
  if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_help ();
  if Array.length Sys.argv <> 3 then print_help ();

  let json_file = Sys.argv.(1) in
  let input = Sys.argv.(2) in
  Printf.printf "Entrée reçue : %s\n\n" input;
  load_machine json_file input