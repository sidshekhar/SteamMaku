open Definitions
open Constants


(* Team data, (each stores a player of course) *)
val team_red: team_data
val team_blue: team_data

(* Game data *)
val game_dt: game_data


(* ========================================================================== *)
(*  handle_time "helper" functions *)

(* Update positions and velocities of all bullets and UFOs *)
val update_bullets: bullet list -> unit
val update_UFOs: ufo list -> unit

(* update positions of all players, taking into account their desired direction and movement mode *)
val update_players: (player_char * player_char) -> unit

(* compile a list of all bullet/player collisions (including grazes) *)
val collide_bullets_players: bullet list -> (player_char * player_char) -> (bullet * player_char) list

(* compile a list of all bullet/UFO collisions  *)
val collide_bullets_UFOs: bullet list -> ufo list -> (bullet * ufo) list

(* process each UFO hit, if destroyed remove and add powers appropriately *)
val process_UFO_hits: (bullet * ufo) list -> unit

(* process each player hit, add graze points for grazes *)
val process_player_hits: (bullet * player_char) list -> unit

(* check player/power collisions, process power collection *)
val process_player_power: unit -> unit

(* check if the game has now ended. Could be by time runnning out or by one player having zero lives left *)
val check_game_ended: unit -> bool


(* ========================================================================== *)
(*  handle_action "helper" functions *)

(* Changes the enqueued moves of the player's character to whatever the argument is *)
val move_player: (direction * direction) list -> unit

(* Performing shooting with the given bullet type, position and acceleration *)
val shoot_com: (bullet_type * position * acceleration) -> unit

(* Change the player's focus type to the value given *)
val focus_player: bool -> unit

