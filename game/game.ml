open Definitions
open Constants
open Util
open State

(* TODO: change this *)
type dirs = {p_red: (direction * direction) list; p_blue: (direction * direction) list}
type game = {game_d: game_data; directions: dirs; time_passed: int}

let init_game () : game =
  let d = {p_red = []; p_blue = []} in
  {game_d= game_dt; directions= d; time_passed= 0}

(* called every timestep to update the server's game object  *)
let handle_time game =
  failwith "what time is it"


let handle_action game col act =
  match act with
  | Move (h::t) -> failwith ""
  | Shoot (a,b,c) -> failwith ""
  | Focus b -> failwith ""
  | Bomb -> failwith ""

let get_data game =
  failwith "I'm the strongest!"

