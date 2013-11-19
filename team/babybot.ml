open Team
open Definitions
open Constants
open Util

(* [receive_data d] is called whenever a game update comes from the server.
 * It's up to you what to do with this update. *)
let receive_data (d : game_data) : unit =
  ()

let () = Random.self_init ()

let count = ref 0

let rand_direction () = let roll = Random.int 8 in
  if roll = 0 then (North, Neutral)
  else if roll = 1 then (North, East)
  else if roll = 2 then (East, Neutral)
  else if roll = 3 then (East, South)
  else if roll = 4 then (South, Neutral)
  else if roll = 5 then (South, West)
  else if roll = 6 then (West, Neutral)
  else (West, North)
  
let rand_loc () =
  let x = Random.float 600. in
  let y = Random.float 600. in
  (x, y)

let rand_accel () =
  let angle = Random.float 360. in
  let magn = Random.float cACCEL_LIMIT in
  (magn *. (cos angle), magn *. (sin angle))

let bot c =
  while true do
    let d = rand_direction () in
    let l = rand_loc () in
    let () = send_action (Move [d;d;d;d]) in
    let () = if !count mod 4 = 0 then
      send_action (Shoot (Spread, l, (rand_accel()))) else () in
    let () = if !count mod 20 = 0 then
      send_action (Shoot (Bubble, l, (rand_accel()))) else () in
    let () = if !count mod 20 = 10 then
      send_action (Shoot (Trail, l, (rand_accel()))) else () in
    let () = if (!count mod 30 = 25 && c = Red) then
      send_action (Bomb) in
    let () = if (!count mod 30 = 10 && c = Blue) then
      send_action (Bomb) in
    incr count;
    Thread.delay 0.20
  done

let () = start_bot bot receive_data
