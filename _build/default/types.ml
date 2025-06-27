type action = Left | Right

type transition = {
  read     : string;
  to_state : string;
  write    : string;
  action   : action;
}

type tape = {
  left    : string list;
  current : string;
  right   : string list;
}

module TransitionMap = Map.Make (String)