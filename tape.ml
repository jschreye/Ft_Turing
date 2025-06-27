open Types

let init_tape input blank =
  match String.to_seq input |> List.of_seq with
  | [] -> { left = []; current = blank; right = [] }
  | h :: t ->
      { left = [];
        current = String.make 1 h;
        right = List.map (fun c -> String.make 1 c) t }

let show_tape t =
  let l = List.rev t.left |> String.concat "" in
  let r = String.concat "" t.right in
  Printf.printf "[%s<%s>%s]\n" l t.current r

let apply_transition blank tape tr =
  let new_sym = tr.write in
  match tr.action with
  | Left ->
      (match tape.left with
       | [] -> { left = []; current = blank; right = new_sym :: tape.right }
       | h :: tl -> { left = tl; current = h; right = new_sym :: tape.right })
  | Right ->
      (match tape.right with
       | [] -> { left = new_sym :: tape.left; current = blank; right = [] }
       | h :: tl -> { left = new_sym :: tape.left; current = h; right = tl })