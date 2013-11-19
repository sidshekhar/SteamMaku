open Definitions
open Constants

let string_of_color : color -> string = function
  | Red -> "Red"
  | Blue -> "Blue"

let string_of_bullet_type : bullet_type -> string = function
  | Spread -> "Spread"
  | Trail -> "Trail"
  | Bubble -> "Bubble"
  | Power -> "Power"

let string_of_result : result -> string = function
  | Winner(c) -> string_of_color c
  | Tie -> "Tie"
  | Unfinished -> "Unfinished" (* Are you sure you want to be doing this? *)

let speed_of_bullet : bullet_type -> int = function
  | Spread -> cSPREAD_SPEED
  | Trail -> cTRAIL_SPEED_STEP
  | Bubble -> cBUBBLE_SPEED
  | Power -> cPOWER_SPEED

let string_of_vector (x, y) =
  "(" ^ (string_of_float x) ^ ", " ^ (string_of_float y) ^ ")"

let radius_of_bullet : bullet_type -> int = function
  | Spread -> cSPREAD_RADIUS
  | Trail -> cTRAIL_RADIUS
  | Bubble -> cBUBBLE_RADIUS
  | Power -> cPOWER_RADIUS

let cost_of_bullet : bullet_type -> int = function
  | Spread -> cSPREAD_COST
  | Trail -> cTRAIL_COST
  | Bubble -> cBUBBLE_COST
  | Power -> max_int

let vector_of_dirs (dirs : direction * direction) (magn : float) =
  let diag = magn /. (sqrt 2.) in
  match dirs with
  | North, East | East, North -> (diag, -.diag)
  | North, West | West, North -> (-.diag, -.diag)
  | South, East | East, South -> (diag, diag)
  | South, West | West, South -> (-.diag, diag)
  | North, _ -> (0., -.magn)
  | South, _ -> (0., magn)
  | East, _ -> (magn, 0.)
  | West, _ -> (-.magn, 0.)
  | Neutral, _ -> (0., 0.)

let id_lock = Mutex.create ()
let next_id = ref 0

let next_available_id () =
  Mutex.lock id_lock;
  incr next_id;
  Mutex.unlock id_lock;
  !next_id

let in_bounds (x, y) =
  (x >= 0. && y >= 0. && x <= float cBOARD_WIDTH && y <= float cBOARD_HEIGHT)

let pi = acos (-1.)

let deg_to_rad x = (180. *. x) /. pi
let rad_to_deg x = (pi *. x) /. 180.

(* add_v v1 v2 returns the vector sum v1 + v2 *)
let add_v (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)

(* subt_v v1 v2 returns the vector difference v1 - v2 *)
let subt_v (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)

let magnitude (x, y) =
  sqrt ((x *. x) +. (y *. y))

let distance v1 v2 =
  magnitude (subt_v v1 v2)

let unit_v (x, y) =
  let magn = magnitude (x, y) in
  (x /. magn, y /. magn)

(* Rotates a vector by theta radians. *)
let rotate (x, y) theta =
  let sint = sin theta in
  let cost = cos theta in
  (x *. cost -. y *. sint, x *. sint +. y *. cost)

(* Rotates a vector by theta degrees. But why would you use degrees? *)
let rotate_deg (x,y) theta = rotate (x,y) (rad_to_deg theta)

let scale s (x, y) =
  (s *. x, s *. y)
