include Util
include Constants
include Definitions



let init_player (col:color) : player_char =
  match col with
  | Red -> {p_id = 0; 
            p_pos = (((float_of_int cBOARD_WIDTH) /. 8.), (float_of_int cBOARD_HEIGHT) /. 2.); 
            p_focused = false;
            p_radius = cHITBOX_RADIUS;
            p_color = Red}
  | Blue -> {p_id = 1; 
            p_pos = ((((float_of_int cBOARD_WIDTH) *. 7.) /. 8.), (float_of_int cBOARD_HEIGHT) /. 2.); 
            p_focused = false;
            p_radius = cHITBOX_RADIUS;
            p_color = Blue}

let init_team (col:color) : team_data =
  match col with
  | Red -> let player = init_player Red in 
            (cINITIAL_LIVES, cINITIAL_BOMBS, 0, 0, 0, player)
  | Blue -> let player = init_player Blue in
            (cINITIAL_LIVES, cINITIAL_BOMBS, 0, 0, 0, player)

let team_red = init_team Red
let team_blue = init_team Blue


let game_dt : game_data = (team_red, team_blue, [], [], [])

let update_bullets b_list = failwith ""

let update_UFOs u_list = failwith ""

let update_players b = failwith ""

let collide_bullets_players b_list plyrs = failwith ""

let collide_bullets_UFOs b_list u_list = failwith ""

let process_UFO_hits bu_list = failwith ""

let process_player_hits bp_list = failwith ""

let process_player_power () = failwith ""

let check_game_ended () = failwith ""


let move_player dd_list = failwith ""

let shoot_com tr = failwith ""

let focus_player b = failwith ""