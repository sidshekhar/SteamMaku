include Util
include Constants
include Definitions
include Netgraphics


type dirs = {
  mutable p_red: (direction * direction) list; 
  mutable p_blue: (direction * direction) list
}

type game = {
  mutable game_d: game_data;
  mutable directions: dirs;
  mutable invincible: (bool * bool); (* (red,blue) *)
  mutable invincible_for: (float * float);
  mutable invincible_frames: (float * float);
  mutable time_passed: float
}

let init_player (col:color) : player_char =
  match col with
  | Red -> let pl = {
            p_id = next_available_id ();
            p_pos = (((float_of_int cBOARD_WIDTH) /. 8.), (float_of_int cBOARD_HEIGHT) /. 2.); 
            p_focused = false;
            p_radius = cHITBOX_RADIUS;
            p_color = Red} in
            add_update (AddPlayer(pl.p_id, pl.p_color, pl.p_pos)); pl
  | Blue -> let pl = {
            p_id = next_available_id ();
            p_pos = ((((float_of_int cBOARD_WIDTH) *. 7.) /. 8.), (float_of_int cBOARD_HEIGHT) /. 2.); 
            p_focused = false;
            p_radius = cHITBOX_RADIUS;
            p_color = Blue} in
            add_update (AddPlayer(pl.p_id, pl.p_color, pl.p_pos)); pl

let init_team (col:color) : team_data =
  match col with
  | Red -> let player = init_player Red in 
            (cINITIAL_LIVES, cINITIAL_BOMBS, 0, 0, 0, player)
  | Blue -> let player = init_player Blue in
            (cINITIAL_LIVES, cINITIAL_BOMBS, 0, 0, 0, player)

let team_red = init_team Red

let team_blue = init_team Blue


let game_dt : game_data = (team_red, team_blue, [], [], [])


let vectors_equal v1 v2 = (add_v v1 (0.,0.) = add_v v2 (0.,0.))

let bullets_equal b1 b2 =
  (string_of_bullet_type b1.b_type = (string_of_bullet_type b2.b_type)) &&
  (b1.b_id = b2.b_id) &&
  (vectors_equal b1.b_pos b2.b_pos) &&
  (vectors_equal b1.b_vel b2.b_vel) &&
  (vectors_equal b1.b_accel b2.b_accel) &&
  (b1.b_radius = b2.b_radius) &&
  (string_of_color b1.b_color = string_of_color b2.b_color)


let update_bullets' (b:bullet) =
  let old_pos = b.b_pos in
  let old_vel = b.b_vel in
  let (v,w) = 
    if (in_bounds (add_v old_pos old_vel))
      then 
      ({b with b_pos= (add_v old_pos old_vel); b_vel= (add_v old_vel b.b_accel)}, true)
    else (b,false) in
  add_update (MoveBullet(b.b_id, if (in_bounds (add_v old_pos old_vel)) then add_v old_pos old_vel else b.b_pos)); (v,w)


let update_bullets b_list : bullet list = 
  List.filter (fun b -> in_bounds b.b_pos) (List.fold_left (fun acc x -> 
      let (a,b) = (update_bullets' x) in 
      if b then a::acc else acc) [] b_list)


let update_UFOs' (u:ufo) = failwith ""
(*  let old_pos = u.u_pos in
  let old_vel = u.u_vel in
  {u with u_pos= (add_v old_pos old_vel); u_vel= (add_v old_vel u.u_accel)}
*)

let update_UFOs u_list = ()
(*  List.filter (fun u -> in_bounds u.u_pos) (List.fold_left (fun acc x -> (update_UFOs' x)::acc) [] u_list);
  ()
*)


let update_players p dir = 
  begin
    if p.p_focused
      then let vect = vector_of_dirs dir (float_of_int cFOCUSED_SPEED) in
      if in_bounds (add_v p.p_pos vect) then {p with p_pos = (add_v p.p_pos vect)}
      else p
    else
      let vect = vector_of_dirs dir (float_of_int cUNFOCUSED_SPEED) in
      if in_bounds (add_v p.p_pos vect) then {p with p_pos = (add_v p.p_pos vect)}
      else p
  end


let update_charge team color =
  let (l1,b1,s1,pw1,c1,pl1) = team in
  match color with
  | Red -> if (c1+cCHARGE_MAX+pw1) <= cCHARGE_MAX
             then c1+cCHARGE_RATE+pw1
           else c1
  | Blue -> if (c1+cCHARGE_MAX+pw1) <= cCHARGE_MAX
              then c1+cCHARGE_RATE+pw1
            else c1



let intersect c1 c2 =
  let (p1,r1) = c1 in
  let (p2,r2) = c2 in
  (distance p1 p2) <= (r1 +. r2)

let collide_bp (b:bullet) (p:player_char) =
  let c1 = (b.b_pos, float_of_int b.b_radius) in
  let c2 = (p.p_pos, float_of_int p.p_radius) in
  b.b_color != p.p_color && 
  intersect c1 c2

let graze' c1 c2 =
  let (p1,r1) = c1 in
  let (p2,r2) = c2 in
  (distance p1 p2) <= (r1 +. r2 +. (float_of_int cGRAZE_RADIUS))

let graze_bp (b:bullet) (p:player_char) =
  let c1 = (b.b_pos, float_of_int b.b_radius) in
  let c2 = (p.p_pos, float_of_int p.p_radius) in
  b.b_color != p.p_color && 
  graze' c1 c2


let collide_bullets_players b_list p = 
  List.fold_left (fun acc x -> if (collide_bp x p) then (x,p,false)::acc 
                               else if (graze_bp x p) then (x,p,true)::acc else acc) [] b_list

let collide_bu b u =
  let c1 = (b.b_pos, float_of_int b.b_radius) in
  let c2 = (u.u_pos, float_of_int u.u_radius) in
  intersect c1 c2

let collide_bu' b_list u =
  List.fold_left (fun acc x -> if (collide_bu x u) then (x,u)::acc else acc) [] b_list

let collide_bullets_UFOs b_list u_list = 
  List.fold_left (fun acc x -> (collide_bu' b_list x) @ acc) [] u_list

(* given the game state and a player,this will tell you if she is invincible *)
let isInvincible (pl:player_char) (gm:game) =
  match pl.p_color with
  | Red -> (fst gm.invincible)
  | Blue -> (snd gm.invincible)


(* clears all bullets from the given game *)
let clear_all_bullets (gm:game) =
  let gdata = gm.game_d in
  let (tr,tb,ul,bl,pwl) = gdata in
  gm.game_d <- (tr,tb,ul,[],pwl);
  List.iter (fun x -> add_update (DeleteBullet(x.b_id))) bl


let flip_color (col:color) : color =
  match col with
  | Red -> Blue
  | Blue -> Red


let process_UFO_hits gamedata bu_list = failwith ""

(* deduct loser's life, clear all bullets, power/2, init bombs, winner's score *)
let scoring gm winner_col grazed : unit = 
  let gdata = gm.game_d in
  let (tr, tb, ul, bl, pwl) = gdata in
    match winner_col with
    | Red -> 
      let (l1,b1,s1,pw1,c1,pl1) = tr in
      let (l2,b2,s2,pw2,c2,pl2) = tb in
      begin
      if grazed 
        then gm.game_d <- ((l1,b1,s1+cKILL_POINTS+cGRAZE_POINTS,pw1,c1,pl1),
                    (l2-1,cINITIAL_BOMBS,s2,pw2/2,c2,pl2), ul, [], pwl)
      else
        gm.game_d <- ((l1,b1,s1+cKILL_POINTS,pw1,c1,pl1),
                    (l2-1,cINITIAL_BOMBS,s2,pw2/2,c2,pl2), ul, [], pwl)
      end;
      gm.invincible <- (true, isInvincible pl2 gm);
      gm.invincible_frames <- ((fst gm.invincible_frames) +. float_of_int cINVINCIBLE_FRAMES, (snd gm.invincible_frames))
    | Blue ->
      let (l1,b1,s1,pw1,c1,pl1) = tr in
      let (l2,b2,s2,pw2,c2,pl2) = tb in
      begin
      if grazed
        then gm.game_d <- ((l1-1,cINITIAL_BOMBS,s1,pw1/2,c1,pl1),
                    (l2,b2,s2+cKILL_POINTS+cGRAZE_POINTS,pw2,c2,pl2), ul, [], pwl)
      else
        gm.game_d <- ((l1-1,cINITIAL_BOMBS,s1,pw1/2,c1,pl1),
                    (l2,b2,s2+cKILL_POINTS,pw2,c2,pl2), ul, [], pwl)
      end;
      gm.invincible <- (isInvincible pl1 gm, true);
      gm.invincible_frames <- ((snd gm.invincible_frames), (snd gm.invincible_frames) +. float_of_int cINVINCIBLE_FRAMES)


let process_player_hits gm (bp_list:(Definitions.bullet * Definitions.player_char * bool) list) = 
  begin List.fold_left 
    (fun acc x -> 
      let (b,p,g) = x in
        scoring acc (flip_color b.b_color) g; acc) gm bp_list
  end;
  let gdata = gm.game_d in
  let (tr,tb,ul,bl,pwl) = gdata in
  let (l1,b1,s1,pw1,c1,pl1) = tr in
  let (l2,b2,s2,pw2,c2,pl2) = tb in
  (l1,l2,s1,s2)


let rec remove_power pw plist = 
  match plist with
  | [] -> plist
  | h::t -> if pw = h then remove_power pw t else t


let collide_players_power p pw = failwith ""

let process_player_power () = failwith ""

let hasLives (tm:team_data) = 
  let (l,_,_,_,_,_) = tm in
  l > 0

let check_game_ended (gm:game) : result = 
  let gdata = gm.game_d in
  let (tr, tb, ul, bl, pwl) = gdata in
  let tim = (gm.time_passed > cTIME_LIMIT) in
  let trhaslives = (hasLives tr) in
  let tbhaslives = (hasLives tb) in 
  match (tim,trhaslives,tbhaslives) with
  | (true,true,true) -> Tie
  | (true,true,false) -> Winner(Red)
  | (true,false,true) -> Winner(Blue)
  | (true,false,false) -> Tie
  | (false,_,_) -> Unfinished
      
  


let init_velocity (targetpos : position) (initpos : position) (speed : int) =
  match targetpos,initpos with 
  | (x, y) -> scale (float_of_int speed) (unit_v (subt_v x y))


(*   *)
let init_velocity_trail (target:position) (initpos:position) (speed:int) (num:int) =
  let vel = scale (float_of_int speed) (unit_v(subt_v target initpos)) in
  match num with
  | 1 -> rotate_deg vel (float_of_int (0 - cTRAIL_ANGLE))
  | 2 -> vel
  | 3 -> rotate_deg vel (float_of_int (cTRAIL_ANGLE))
  | _ -> vel


let init_velocity_spread target initpos speed num =
  let vel = scale (float_of_int speed) (unit_v(subt_v target initpos)) in
  rotate_deg vel (float_of_int num *. (360. /. (float_of_int cSPREAD_NUM)))

let trail_creator game col accel target_loc =  
  let gdata = game.game_d in
  let (tr, tb, ul, bl, pwl) = gdata in
  let (l1,b1,s1,pw1,c1,pl1) = tr in
  let (l2,b2,s2,pw2,c2,pl2) = tb in
  let bulletlst = ref [] in
  if col == Red then let t = ref 1 in
    while !t < 4 do 
      let i = ref 1 in
        while !i < (cTRAIL_NUM + 1) do 
          bulletlst := {
          b_id = next_available_id ();
          b_type = Trail;
          b_pos = pl1.p_pos;
          b_vel = init_velocity_trail target_loc pl1.p_pos (cTRAIL_SPEED_STEP * !i) (!t);
          b_accel = if (magnitude accel) < cACCEL_LIMIT then accel else (0., 0.);
          b_radius = cTRAIL_RADIUS;
          b_color = Red}::(!bulletlst);
          i := !i + 1;
          let nb = List.hd (!bulletlst) in
          add_update (AddBullet(nb.b_id, nb.b_color, nb.b_type, nb.b_pos));
        done;
      t := !t + 1;
    done;
    let ngdata = (tr,tb,ul,(bl @ !bulletlst), pwl) in
    game.game_d <- ngdata
  else
    let t = ref 1 in
    while !t < 4 do 
      let i = ref 1 in
        while !i < (cTRAIL_NUM + 1) do 
          bulletlst := {
          b_id = next_available_id ();
          b_type = Trail;
          b_pos = pl2.p_pos;
          b_vel = init_velocity_trail target_loc pl2.p_pos (cTRAIL_SPEED_STEP * !i) (!t);
          b_accel = if (magnitude accel) < cACCEL_LIMIT then accel else (0., 0.);
          b_radius = cTRAIL_RADIUS;
          b_color = Blue}::(!bulletlst);
          i := !i + 1;
          let nb = List.hd (!bulletlst) in
          add_update (AddBullet(nb.b_id, nb.b_color, nb.b_type, nb.b_pos));
        done;
      t := !t + 1;
    done;
    let ngdata = (tr,tb,ul,(bl @ !bulletlst), pwl) in
    game.game_d <- ngdata


let spread_creator game col accel target_loc =
  let gdata = game.game_d in
  let (tr, tb, ul, bl, pwl) = gdata in
  let (l1,b1,s1,pw1,c1,pl1) = tr in
  let (l2,b2,s2,pw2,c2,pl2) = tb in
  let bulletlst = ref [] in
  if col == Red then let i = ref 0 in
    while !i < (cSPREAD_NUM + 1) do 
      bulletlst := {
      b_id = next_available_id ();
      b_type = Spread;
      b_pos = pl1.p_pos;
      b_vel = init_velocity_spread target_loc pl1.p_pos cSPREAD_SPEED !i;
      b_accel = if (magnitude accel) < cACCEL_LIMIT then accel else (0., 0.);
      b_radius = cSPREAD_RADIUS;
      b_color = Red}::(!bulletlst);
      i := !i + 1;
      let nb = List.hd (!bulletlst) in
      add_update (AddBullet(nb.b_id, nb.b_color, nb.b_type, nb.b_pos));
    done;
    let ngdata = (tr,tb,ul,(bl @ !bulletlst), pwl) in
    game.game_d <- ngdata
  else
    let i = ref 0 in
    while !i < (cSPREAD_NUM + 1) do 
      bulletlst := {
      b_id = next_available_id ();
      b_type = Spread;
      b_pos = pl2.p_pos;
      b_vel = init_velocity_spread target_loc pl2.p_pos cSPREAD_SPEED !i;
      b_accel = if (magnitude accel) < cACCEL_LIMIT then accel else (0., 0.);
      b_radius = cSPREAD_RADIUS;
      b_color = Blue}::(!bulletlst);
      i := !i + 1;
      let nb = List.hd (!bulletlst) in

      add_update (AddBullet(nb.b_id, nb.b_color, nb.b_type, nb.b_pos));
    done;
    let ngdata = (tr,tb,ul,(bl @ !bulletlst), pwl) in
    game.game_d <- ngdata
