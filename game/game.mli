open Constants
open Definitions

type game

(* [init_game ()] returns a new value of type game, which reflects
 * the initial state of the game. *)
val init_game : unit -> game

(* [handle_time game] advances the state of the game by one time step,
 * returning the resulting new state. If one team won during this time step,
 * then the winning team is returned as the result; if there was a tie, then
 * Tie is returned; else, Unfinished is returned. *)

val handle_time : game -> (game * result)

(* [handle_action game c act] changes the state of the game to reflect 
 * the action act, taken by the team with color c. It returns the new game 
 * state. *)

val handle_action : game -> color -> action -> game

(* [get_data game] returns a game_data value that accurately reflects the
 * state of the game. *)

val get_data : game -> game_data

