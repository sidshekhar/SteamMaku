open Definitions
open Constants
open Util
open Netgraphics
open State
include Util

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

let current_dirs : dirs = {
  p_red = [];
  p_blue = []
}


let current_game : game = 
  {game_d = game_dt; 
   directions = current_dirs;
   invincible = (false, false);
   invincible_for = (0., 0.);
   invincible_frames = (0., 0.);
   time_passed = 0.0}

let init_game () : game =
   current_game.game_d <- game_dt; 
   current_game.directions <- current_dirs;
   current_game.invincible <- (false, false); 
   current_game.invincible_for <- (0., 0.);
   current_game.invincible_frames <- (0., 0.);
   current_game.time_passed <- 0.0;
   current_game.invincible_frames <- (0., 0.);
   current_game

let pop_direction dirlist col : (direction * direction) =
  match dirlist with
  | [] -> (Neutral, Neutral)
  | h::t -> if col = Red then begin current_dirs.p_red <- t; h end
            else begin current_dirs.p_blue <- t; h end


let fix_inv game : unit =
  let (ir,ib) = game.invincible in
  let (irf, ibf) = game.invincible_for in
  let bl1 = (irf > 0.) && (ibf == 0.) in
  let bl2 = (irf > 0.) && (ibf > 0.) in 
  let bl3 = (irf == 0.) && (ibf > 0.) in
  let bl4 = (irf == 0.) && (ibf == 0.) in
    match (bl1,bl2,bl3,bl4) with
    | (true,false,false,false) -> game.invincible <- (ir, false);
          game.invincible_for <- ((irf -. cUPDATE_TIME), ibf)
    | (false,true,false,false) -> game.invincible <- (ir, ib);
          game.invincible_for <- (irf -. cUPDATE_TIME, ibf -. cUPDATE_TIME)
    | (false,false,true,true) -> game.invincible <- (false, ib);
          game.invincible_for <- (irf, ibf -. cUPDATE_TIME)
    | (false,false,false,true) -> game.invincible <- (false, false)
    | _ -> ()

let fix_inv2 game : unit =
  let (ir,ib) = game.invincible in
  let (irf, ibf) = game.invincible_frames in
  let bl1 = (irf > 0.) && (ibf == 0.) in
  let bl2 = (irf > 0.) && (ibf > 0.) in 
  let bl3 = (irf == 0.) && (ibf > 0.) in
  let bl4 = (irf == 0.) && (ibf == 0.) in
    match (bl1,bl2,bl3,bl4) with
    | (true,false,false,false) -> game.invincible <- (ir, false);
          game.invincible_for <- ((irf -. cUPDATE_TIME), ibf)
    | (false,true,false,false) -> game.invincible <- (ir, ib);
          game.invincible_for <- (irf -. cUPDATE_TIME, ibf -. cUPDATE_TIME)
    | (false,false,true,true) -> game.invincible <- (false, ib);
          game.invincible_for <- (irf, ibf -. cUPDATE_TIME)
    | (false,false,false,true) -> game.invincible <- (false, false)
    | _ -> ()




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

  let new_bl = update_bullets bl in
  update_UFOs ul;

  let new_pr = update_players pl1 (pop_direction game.directions.p_red Red) in
  let new_pb = update_players pl2 (pop_direction game.directions.p_blue Blue) in

  add_update (MovePlayer(new_pr.p_id, new_pr.p_pos));
  add_update (MovePlayer(new_pb.p_id, new_pb.p_pos));

  let bpr_collisions = 
    let ans = collide_bullets_players new_bl new_pr in 
    if (List.length ans) > 0 then (List.hd (collide_bullets_players new_bl new_pr))::[]
    else [] in
  let bpbl_collisions = 
    let ans' = collide_bullets_players new_bl new_pb in 
    if (List.length ans') > 0 then (List.hd (collide_bullets_players new_bl new_pb))::[]
    else [] in
  let (l1,l2,s1,s2) = process_player_hits game (bpr_collisions @ bpbl_collisions) in
  clear_all_bullets game;
  
  add_update (SetLives(Red, l1)); 
  add_update (SetLives(Blue, l2));
  add_update (SetScore(Red, s1)); 
  add_update (SetScore(Blue, s2));
  
  let result = check_game_ended game in
  game.time_passed <- game.time_passed +. cUPDATE_TIME;
  
  let new_rc = update_charge tr Red in
  let new_rb = update_charge tr Blue in
  let new_tr = (l1,b1,s1,pw1,new_rc,new_pr) in
  let new_tb = (l2,b2,s2,pw2,new_rb,new_pb) in
  let new_gdata = (new_tr, new_tb, ul, new_bl, pwl) in

  add_update (SetBombs(Red, b1));
  add_update (SetBombs(Blue, b2));
  add_update (SetCharge(Red, new_rc));
  add_update (SetCharge(Blue, new_rb));
  add_update (SetPower(Red, pw1));
  add_update (SetPower(Blue, pw2));

  fix_inv game;
  fix_inv2 game;

  game.game_d <- new_gdata;
  current_game.game_d <- game.game_d;
  current_game.directions <- game.directions;
  current_game.invincible <- game.invincible;
  current_dirs.p_red <- game.directions.p_red;
  current_dirs.p_blue <- game.directions.p_blue;
  current_game.time_passed <- game.time_passed;
  (game, result)


let handle_action game col act =
  let gdata = game.game_d in
  let (tr, tb, ul, bl, pwl) = gdata in
  let (l1,b1,s1,pw1,c1,pl1) = tr in
  let (l2,b2,s2,pw2,c2,pl2) = tb in
  match act with
  | Move l -> 
    if (col == Red) 
      then begin current_dirs.p_red <- l; game.directions <- current_dirs; game end
    else begin current_dirs.p_blue <- l; game.directions <- current_dirs; game end
  | Shoot (shot_type,target_loc,accel) -> 
      if col == Red 
        then begin match shot_type with
          | Bubble -> let newbulletlst = ({
            b_id = next_available_id (); 
            b_type = Bubble;
            b_pos = pl1.p_pos;
            b_vel = init_velocity target_loc pl1.p_pos cBUBBLE_SPEED;
            b_accel = if (magnitude accel) < cACCEL_LIMIT then accel else (0., 0.); b_radius = cBUBBLE_RADIUS; 
            b_color = Red})::bl in 
            let newgdata = (tr, tb, ul, newbulletlst, pwl) in
            let hed = List.hd newbulletlst in
            add_update (AddBullet(hed.b_id, Red, Bubble, hed.b_pos));
            game.game_d <- newgdata; game
          | Trail -> trail_creator game col accel target_loc; game
          | Spread -> spread_creator game col accel target_loc; game
        end
     else begin match shot_type with
          | Bubble -> let newbulletlst = ({
            b_id = next_available_id (); 
            b_type = Bubble;
            b_pos = pl1.p_pos;
            b_vel = init_velocity target_loc pl1.p_pos cBUBBLE_SPEED;
            b_accel = if (magnitude accel) < cACCEL_LIMIT then accel else (0., 0.); b_radius = cBUBBLE_RADIUS; 
            b_color = Blue})::bl in 
            let newgdata = (tr, tb, ul, newbulletlst, pwl) in 
            let hed = List.hd newbulletlst in
            add_update (AddBullet(hed.b_id, Blue, Bubble, hed.b_pos));
            game.game_d <- newgdata; game
          | Trail -> trail_creator game col accel target_loc; game
          | Spread -> spread_creator game col accel target_loc; game
          end

  | Focus b -> let (rd,bl) = game.invincible in
      if (col == Red) then begin game.invincible <- (b,bl); game end
      else begin game.invincible <- (rd,b); game end
  | Bomb -> 
    if col = Red then
      if b1 > 0 
        then begin
        add_update (UseBomb(col));
        let (r,b) = game.invincible in
        let (ir,ib) = game.invincible_for in
        game.invincible <- (true, b);
        game.invincible_for <- (ir +. (float_of_int cBOMB_DURATION), ib);
        add_update (UseBomb(col));
        let newtr = (l1,b1-1,s1,pw1,c1,pl1) in
        let newtb = (l2,b2,s2,pw2,c2,pl2) in
        let new_G = (newtr, newtb, ul,[],pwl) in
        game.game_d <- new_G;
        (*clear_all_bullets game;*)
        game
        end
      else game
    else if b2 > 0
      then begin
      add_update (UseBomb(col));
      let (r,b) = game.invincible in
      let (ir,ib) = game.invincible_for in
      game.invincible <- (r, true);
      game.invincible_for <- (ir, ib +. float_of_int cBOMB_DURATION);
      add_update (UseBomb(col));
      let newtr = (l1,b1,s1,pw1,c1,pl1) in
      let newtb = (l2,b2-1,s2,pw2,c2,pl2) in
      let new_G = (newtr, newtb, ul,[],pwl) in
      game.game_d <- new_G;
      (*clear_all_bullets game;*)
      game
      end
    else game
    

let get_data game =
  game.game_d


(* 
  * Clear all bullets
  * SetBombs
  * player invincible for cBOMB_DURATION

 *)