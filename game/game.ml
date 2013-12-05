open Definitions
open Constants
open Util
open Netgraphics
open State

(*
type dirs = {
  mutable p_red: (direction * direction) list; 
  mutable p_blue: (direction * direction) list
}
*)
(*
type game = {
  mutable game_d: game_data; 
  mutable directions: dirs;
  mutable invincible: (bool * bool); (* (red,blue) *)
  mutable time_passed: float
}
*)
type dirs = State.dirs
type game = State.game

let dirs : dirs = {
  p_red = [(North, East); (North, East)];
  p_blue = []
}


let current_game : game = 
  {game_d = game_dt; 
   directions = dirs;
   invincible = (false, false); 
   time_passed = 0.0}

let init_game () : game =
   current_game.game_d <- game_dt; 
   current_game.directions <- dirs;
   current_game.invincible <- (false, false); 
   current_game.time_passed <- 0.0;
   current_game

let pop_direction dirlist col : (direction * direction) =
  match dirlist with
  | [] -> (Neutral, Neutral)
  | h::t -> if col = Red then begin dirs.p_red <- t; h end
            else begin dirs.p_blue <- t; h end


let showPlayer p = 
  "{id = " ^ (string_of_int p.p_id) ^ ",\n" ^ "position = " ^ 
  (string_of_vector p.p_pos) ^
  ",\n" ^ "focused = " ^ (string_of_bool p.p_focused) ^ ",\n" ^
  "radius = " ^ (string_of_int p.p_radius) ^ ",\n" ^ "color = " ^ 
  (string_of_color p.p_color) ^ "\n" ^ "}\n"


(* called every timestep to update the server's game object  *)
let handle_time game =
  (*  
   *  
   *  update bullet and UFO positions
   *  update player positions
   *  compile list of all bullet/player collisions
   *  compile list of all bullet/UFO collisions
   *  __process UFO hits
   *  process player hits
   *  __compile player/power collisions
   *  __process player power collisions
   *  check if game has ended
   *  increment time_passed
   *
 *)
  
  let gdata = game.game_d in
  let (tr, tb, ul, bl, pwl) = gdata in
  let (l1,b1,s1,pw1,c1,pl1) = tr in
  let (l2,b2,s2,pw2,c2,pl2) = tb in

  update_bullets bl;
  update_UFOs ul;

  let new_pr = update_players pl1 (pop_direction game.directions.p_red Red) in
  let new_pb = update_players pl2 (pop_direction game.directions.p_blue Blue) in

  add_update (MovePlayer(new_pr.p_id, new_pr.p_pos));
  add_update (MovePlayer(new_pb.p_id, new_pb.p_pos));

  let bpr_collisions = collide_bullets_players bl new_pr in
  let bpbl_collisions = collide_bullets_players bl new_pb in
  let bufo_collisions = collide_bullets_UFOs bl ul in
  process_player_hits game bpr_collisions;
  process_player_hits game bpbl_collisions;
  let result = check_game_ended game in
  game.time_passed <- game.time_passed +. cUPDATE_TIME;
  
  let new_tr = (l1,b1,s1,pw1,c1,new_pr) in
  let new_tb = (l2,b2,s2,pw2,c2,new_pb) in
  let new_gdata = (new_tr, new_tb, ul, bl, pwl) in
  game.game_d <- new_gdata;
  current_game.game_d <- game.game_d;
  current_game.directions <- game.directions;
  current_game.invincible <- game.invincible;
  dirs.p_red <- game.directions.p_red;
  dirs.p_blue <- game.directions.p_blue;
  current_game.time_passed <- game.time_passed;
  (game, result)


let handle_action game col act =
  match act with
  | Move l -> 
    if (col == Red) 
      then begin dirs.p_red <- l; game.directions <- dirs; game end
    else begin dirs.p_blue <- l; game.directions <- dirs; game end
  | Shoot (a,b,c) -> failwith "Not shooting yet"
  | Focus b -> let (rd,bl) = game.invincible in
      if (col == Red) then begin game.invincible <- (b,bl); game end
      else begin game.invincible <- (rd,b); game end
  | Bomb -> failwith "not bombing yet"

let get_data game =
  game.game_d

