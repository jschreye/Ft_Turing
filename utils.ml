let count_char c s =
  String.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0 s