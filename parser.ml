open Types
open Yojson.Basic.Util

module StringSet = Set.Make(String)

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

let validate_machine ~alphabet ~states ~initial ~finals ~blank ~transitions =
  let alphabet_set = List.to_seq alphabet |> StringSet.of_seq in
  let states_set = List.to_seq states |> StringSet.of_seq in

  (* Vérification longueur des symboles *)
  List.iter (fun sym ->
    if String.length sym <> 1 then begin
      Printf.eprintf "❌ Erreur : symbole d’alphabet '%s' doit être de longueur 1\n" sym;
      exit 1
    end
  ) alphabet;

  (* Vérification que blank est dans alphabet *)
  if not (StringSet.mem blank alphabet_set) then begin
    Printf.eprintf "❌ Erreur : le symbole blanc '%s' n'est pas dans l'alphabet\n" blank;
    exit 1
  end;

  (* Vérification état initial *)
  if not (StringSet.mem initial states_set) then begin
    Printf.eprintf "❌ Erreur : état initial invalide : '%s'\n" initial;
    exit 1
  end;

  (* Vérification états finaux *)
  List.iter (fun s ->
    if not (StringSet.mem s states_set) then begin
      Printf.eprintf "❌ Erreur : état final inconnu : '%s'\n" s;
      exit 1
    end
  ) finals;

  (* Vérification que chaque état a des transitions *)
  List.iter (fun state ->
    if not (TransitionMap.mem state transitions) then begin
      Printf.eprintf "❌ Erreur : l’état '%s' n’a pas de transitions définies\n" state;
      exit 1
    end
  ) states;

  (* Vérification contenu des transitions *)
  TransitionMap.iter (fun state trs ->
    if not (StringSet.mem state states_set) then begin
      Printf.eprintf "❌ Erreur : transitions définies pour un état inconnu : '%s'\n" state;
      exit 1
    end;
    List.iter (fun t ->
      if not (StringSet.mem t.read alphabet_set) then begin
        Printf.eprintf "❌ Erreur : symbole inconnu en lecture : '%s'\n" t.read;
        exit 1
      end;
      if not (StringSet.mem t.write alphabet_set) then begin
        Printf.eprintf "❌ Erreur : symbole inconnu en écriture : '%s'\n" t.write;
        exit 1
      end;
      if not (StringSet.mem t.to_state states_set) then begin
        Printf.eprintf "❌ Erreur : transition vers un état inconnu : '%s'\n" t.to_state;
        exit 1
      end
    ) trs
  ) transitions