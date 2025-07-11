open Types

(* -------------------------------------------------------------------------- *)
(*  Initialisation du ruban                                                   *)
(* -------------------------------------------------------------------------- *)
let init_tape input blank =
  match String.to_seq input |> List.of_seq with
  | [] ->
      (* Ruban vide : la tête est sur le blanc *)
      { left = []; current = blank; right = [] }
  | h :: t ->
      (* On place la tête sur le premier symbole, le reste à droite *)
      { left = [];
        current = String.make 1 h;
        right = List.map (fun c -> String.make 1 c) t }

(* -------------------------------------------------------------------------- *)
(*  Affichage du ruban                                                         *)
(* -------------------------------------------------------------------------- *)
let show_tape t =
  (* Concatène la partie gauche (inversée), le symbole courant et la partie droite *)
  let l = List.rev t.left |> String.concat "" in
  let r = String.concat "" t.right in
  Printf.printf "[%s<%s>%s]\n" l t.current r

(* -------------------------------------------------------------------------- *)
(*  Application d'une transition                                               *)
(* -------------------------------------------------------------------------- *)
let apply_transition blank tape tr =
  let new_sym = tr.write in
  match tr.action with
  | Left ->
      (* Déplacement vers la gauche : on récupère le symbole à gauche ou le blanc *)
      (match tape.left with
       | [] -> { left = []; current = blank; right = new_sym :: tape.right }
       | h :: tl -> { left = tl; current = h; right = new_sym :: tape.right })
  | Right ->
      (* Déplacement vers la droite : on récupère le symbole à droite ou le blanc *)
      (match tape.right with
       | [] -> { left = new_sym :: tape.left; current = blank; right = [] }
       | h :: tl -> { left = new_sym :: tape.left; current = h; right = tl })
