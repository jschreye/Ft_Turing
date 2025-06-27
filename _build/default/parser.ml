open Types
open Yojson.Basic.Util

let action_of_string = function
  | "LEFT" -> Left
  | "RIGHT" -> Right
  | s -> failwith ("Action inconnue : " ^ s)

let parse_transitions json =
  let trs_json = json |> member "transitions" |> to_assoc in
  List.fold_left (fun acc (state, lst) ->
    let transitions = lst |> to_list |> List.map (fun t ->
      {
        read     = t |> member "read"     |> to_string;
        to_state = t |> member "to_state" |> to_string;
        write    = t |> member "write"    |> to_string;
        action   = t |> member "action"   |> to_string |> action_of_string;
      }
    ) in
    TransitionMap.add state transitions acc
  ) TransitionMap.empty trs_json