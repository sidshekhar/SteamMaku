open Definitions
open Constants


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

(* Team data, (each stores a player of course) *)
val team_red: team_data
val team_blue: team_data

(* Game data *)
val game_dt: game_data


(* ========================================================================== *)
(*  handle_time "helper" functions *)

(* Update positions and velocities of all bullets and UFOs *)
val update_bullets: bullet list -> bullet list
val update_UFOs: ufo list -> unit

(* update positions of all players, taking into account their desired direction and movement mode *)
val update_players: player_char -> (direction * direction) -> player_char

(* updates the players' charge every timestep, returns the new charge *)
val update_charge: team_data -> color -> int

(* compile a list of all bullet/player collisions (including grazes) *)
val collide_bullets_players: bullet list -> player_char -> (bullet * player_char * bool) list

(* compile a list of all bullet/UFO collisions  *)
val collide_bullets_UFOs: bullet list -> ufo list -> (bullet * ufo) list

(* process each UFO hit, if destroyed remove and add powers appropriately *)
val process_UFO_hits: game -> (bullet * ufo) list -> unit

(* process each player hit, add graze points for grazes *)
val process_player_hits: game -> (bullet * player_char * bool) list -> (int * int * int * int)

(* compile a list of all player/power collisions *)
val collide_players_power: player_char -> power -> (player_char * power) list

(* check player/power collisions, process power collection *)
val process_player_power: unit -> unit

(* clears the screen of all bullets *)
val clear_all_bullets: game -> unit

(* check if the game has now ended. Could be by time runnning out or by one player having zero lives left *)
val check_game_ended: game -> result


(* ========================================================================== *)
(*  handle_action "helper" functions *)

(* returns the initial velocity of a bullet given the target and init positions *)
val init_velocity: position -> position -> int -> velocity

(* returns the initial velocity of a spread bullet  *)
val init_velocity_spread: position -> position -> int -> int -> velocity

(* creates a list of spread bullets and returns the new game data *)
val trail_creator: game -> color -> vector -> vector -> unit

val spread_creator: game -> color -> vector -> vector -> unit