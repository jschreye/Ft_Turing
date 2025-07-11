open Types
open Tape

(* -------------------------------------------------------------------------- *)
(*  Recherche d’une transition applicable                                     *)
(* -------------------------------------------------------------------------- *)
let find_transition trs state symbol =
  try
    let options = TransitionMap.find state trs in
    (* On cherche la première transition dont le champ [read] correspond *)
    Some (List.find (fun t -> t.read = symbol) options)
  with Not_found ->
    None

(* -------------------------------------------------------------------------- *)
(*  Exécution d’une seule étape de la machine                                 *)
(* -------------------------------------------------------------------------- *)
let step blank trs state tape step_count =
  match find_transition trs state tape.current with
  | None ->
      (* Pas de transition correspondante → blocage *)
      Printf.printf "\n❌ Aucun passage défini pour (%s, '%s')\n"
        state tape.current;
      None
  | Some tr ->
      (* Journalisation de l’étape *)
      Printf.printf "\n--- Étape %d ---\n" step_count;
      Printf.printf "État actuel : %s\n" state;
      Printf.printf "Symbole lu : '%s'\n" tr.read;
      Printf.printf
        "Action : écrire '%s', aller à l'état %s, déplacer tête vers %s\n"
        tr.write tr.to_state
        (match tr.action with Left -> "gauche" | Right -> "droite");
      show_tape tape;

      (* Application de la transition *)
      let tape' = apply_transition blank tape tr in
      Printf.printf "Nouvel état de la bande après transition :\n";
      show_tape tape';

      Some (tr.to_state, tape')

(* -------------------------------------------------------------------------- *)
(*  Boucle de simulation complète                                             *)
(* -------------------------------------------------------------------------- *)
let rec run ~blank ~trs ~finals ~state ~tape ~steps_left step_count =
  (* 1) Détection de dépassement de pas maximum *)
  if steps_left = 0 then (
    Printf.printf "\n❌ Machine bloquée : trop d’étapes sans HALT\n";
    show_tape tape;
    exit 1
  );

  (* 2) Vérification de l’état final *)
  if List.mem state finals then (
    Printf.printf "\n✅ HALT dans l’état %s après %d étapes\n" state step_count;
    show_tape tape
  ) else

  (* 3) Exécution d’une étape et récursion *)
    match step blank trs state tape step_count with
    | None ->
        (* Blocage en cours de simulation *)
        Printf.printf
          "\n❌ Machine bloquée dans l’état %s (aucune transition applicable)\n"
          state;
        show_tape tape;
        exit 1
    | Some (state', tape') ->
        (* Poursuite de la simulation *)
        run
          ~blank
          ~trs
          ~finals
          ~state:state'
          ~tape:tape'
          ~steps_left:(steps_left - 1)
          (step_count + 1)
