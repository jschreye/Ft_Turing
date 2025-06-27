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

let rec run ~blank ~trs ~finals ~state ~tape ~steps_left =
  if steps_left = 0 then (
    Printf.printf "❌ Machine bloquée : trop d’étapes sans HALT\n";
    show_tape tape;
    exit 1
  );
  if List.mem state finals then (
    Printf.printf "✅ HALT dans l’état %s\n" state;
    show_tape tape
  ) else
    match step blank trs state tape with
    | None ->
        Printf.printf "❌ Machine bloquée dans l’état %s (aucune transition applicable)\n" state;
        show_tape tape;
        exit 1
    | Some (state', tape') ->
        run ~blank ~trs ~finals ~state:state' ~tape:tape' ~steps_left:(steps_left - 1)
  
let rec run_machine blank transitions finals state tape =
  if List.mem state finals then (
    Printf.printf "✅ Machine terminée (état final : %s)\n" state;
    show_tape tape
  ) else
    match find_transition transitions state tape.current with
    | None ->
        Printf.printf "❌ Machine bloquée (aucune transition pour (%s, '%s'))\n" state tape.current;
        show_tape tape
    | Some tr ->
        Printf.printf "(%s, %s) -> (%s, %s, %s)\n"
          state tr.read tr.to_state tr.write
          (match tr.action with Left -> "LEFT" | Right -> "RIGHT");
        let tape' = apply_transition blank tape tr in
        show_tape tape';
        run_machine blank transitions finals tr.to_state tape'