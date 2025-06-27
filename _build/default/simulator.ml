open Types
open Tape

let find_transition trs state symbol =
  try
    let options = TransitionMap.find state trs in
    Some (List.find (fun t -> t.read = symbol) options)
  with Not_found -> None

let step blank trs state tape =
  match find_transition trs state tape.current with
  | None ->
      Printf.printf "❌ Aucun passage défini pour (%s, '%s')\n" state tape.current;
      None
  | Some tr ->
      Printf.printf "(%s, %s) -> (%s, %s, %s)\n"
        state tr.read tr.to_state tr.write
        (match tr.action with Left -> "LEFT" | Right -> "RIGHT");
      let tape' = apply_transition blank tape tr in
      show_tape tape';
      Some (tr.to_state, tape')

let rec run blank trs finals state tape =
  if List.mem state finals then
    Printf.printf "✅ HALT dans l’état %s\n" state
  else
    match step blank trs state tape with
    | None -> Printf.printf "❌ Machine bloquée dans l’état %s\n" state
    | Some (state', tape') -> run blank trs finals state' tape'