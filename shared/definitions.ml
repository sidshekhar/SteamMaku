type color = Red | Blue

type result = Winner of color | Tie | Unfinished

(* think of these as arrow keys *)
type direction = North | South | East | West | Neutral

type vector = float * float

type velocity = vector
type position = vector
type acceleration = vector

type id = int

type bullet_type = Spread | Bubble | Power | Trail

type unit_type = Player of color
                | Bullet of (bullet_type * color)
                | UFO

type bullet = {
  b_type : bullet_type;
  b_id : int;
  b_pos : position;
  b_vel : velocity;
  b_accel : acceleration;
  b_radius : int;
  b_color : color
}

type power = bullet

type player_char = {
  p_id : int;
  p_pos : position;
  p_focused : bool;
  p_radius : int;
  p_color : color
}

type ufo = {
  u_id : int;
  u_pos : position;
  u_vel : velocity;
  u_radius : int;
  u_red_hits : int;
  u_blue_hits : int
}

(* lives, bombs, score, power, charge, player *)
type team_data = int * int * int * int * int * player_char

(* red team, blue team, npcs, bullets, powerups *)
type game_data = team_data * team_data * ufo list * bullet list * power list

(* Graphics updates *)

(* InitGraphics list:
  * [width; height; spread radius; laser width; bubble radius;
  *  initial lives; initial bombs] 
  *
  *  (students, you don't need to use InitGraphics) *)
type update = InitGraphics of int list
              | AddPlayer of (id * color * position)
              | AddBullet of (id * color * bullet_type * position)
              | AddUFO of (id * position)
              | Graze
              | MovePlayer of (id * position)
              | MoveBullet of (id * position)
              | MoveUFO of (id * position)
              | DeleteBullet of id
              | DeleteUFO of id
              | SetBombs of (color * int)
              | UseBomb of color
              | SetLives of (color * int)
              | SetScore of (color * int)
              | SetPower of (color * int)
              | SetCharge of (color * int)
              | Countdown of int
              | GameOver of result

(* Game commands *)

type control = GameStart
              | GameRequest
              | Team of color
              | GameEnd

type action = Move of (direction * direction) list
              | Shoot of bullet_type * position * acceleration
              | Focus of bool
              | Bomb

type command = Control of control 
              | Action of action
              | Error of string
			        | Result of result
              | Data of game_data
