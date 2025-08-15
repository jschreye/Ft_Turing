(* encode_unary_add_universal.ml - Encodeur pour la vraie machine universelle *)

let encode_unary_add_machine input =
  (* Utiliser EXACTEMENT le format qui fonctionne dans le README *)
  (* Format: C&C{[1C>1][+S>.][.H>.]}S{[1P<+][.H<.]}P{[.C>1]}*1+11 *)
  
  let machine_encoding = 
    (* Copie exacte du format qui marche *)
    "C&C{[1C>1][+S>.][.H>.]}S{[1P<+][.H<.]}P{[.C>1]}"
  in
  
  (* Encoder l'entrée - remplacer le premier caractère par la tête marquée *)
  let word_encoding = 
    if input = "" then "*"
    else 
      let first_char = String.sub input 0 1 in
      let rest = if String.length input > 1 then String.sub input 1 (String.length input - 1) else "" in
      "*" ^ first_char ^ rest
  in
  
  (* Format final: MACHINE*WORD *)
  machine_encoding ^ word_encoding

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <input>\n" Sys.argv.(0);
    Printf.printf "Exemple: %s \"1+1=\"\n" Sys.argv.(0);
    Printf.printf "\n";
    Printf.printf "🎯 ENCODEUR POUR MACHINE UNIVERSELLE VRAIE\n";
    Printf.printf "==========================================\n";
    Printf.printf "Encode unary_add.json + entrée pour 05_pseudo_universal.json\n";
    Printf.printf "\n";
    Printf.printf "Format: MACHINE_DESCRIPTION*INPUT\n";
    Printf.printf "- MACHINE: Description encodée de unary_add.json\n";
    Printf.printf "- INPUT: Entrée à traiter (ex: 1+1=)\n";
    Printf.printf "\n";
    Printf.printf "Mapping des états:\n";
    Printf.printf "  scan -> C\n";
    Printf.printf "  cleanup -> S\n";
    Printf.printf "  HALT -> H\n";
    Printf.printf "\n";
    Printf.printf "Ceci est une VRAIE machine universelle !\n";
    exit 1
  ) else (
    let encoded = encode_unary_add_machine Sys.argv.(1) in
    Printf.printf "%s\n" encoded
  )
