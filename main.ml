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
    let json = Yojson.Basic.from_file json_file in
    let name = json |> member "name" |> to_string in
  
    if name = "unary_sub" then begin
      (try
         let dash = String.index input '-' in
         let eq   = String.index input '=' in
         let left  = String.sub input 0 dash in
         let right = String.sub input (dash + 1) (eq - dash - 1) in
         let count_ones s =
           String.fold_left (fun acc c -> if c = '1' then acc + 1 else acc) 0 s
         in
         let nl = count_ones left in
         let nr = count_ones right in
         if nl < nr then (
           Printf.eprintf
             "❌ Erreur : soustraction impossible %d - %d (underflow)\n"
             nl nr;
           exit 1
         )
       with Not_found ->
         ()
      )
    end;
  
    let alphabet = json |> member "alphabet" |> to_list |> filter_string in
    let initial  = json |> member "initial"  |> to_string in
    let finals   = json |> member "finals"   |> to_list |> filter_string in
    let blank    = json |> member "blank"    |> to_string in
    let trs      = parse_transitions json in
    let states   = json |> member "states"   |> to_list |> filter_string in
  
    validate_machine ~alphabet ~states ~initial ~finals ~blank ~transitions:trs;
  
    Printf.printf "Nom de la machine : %s\n" name;
    let tape0 = init_tape input blank in
    run ~blank ~trs ~finals ~state:initial ~tape:tape0 ~steps_left:1000 1

let () =
  if Array.length Sys.argv < 2 then print_help ();
  if Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" then print_help ();
  if Array.length Sys.argv <> 3 then print_help ();

  let json_file = Sys.argv.(1) in
  let input = Sys.argv.(2) in
  Printf.printf "Entrée reçue : %s\n\n" input;
  load_machine json_file input