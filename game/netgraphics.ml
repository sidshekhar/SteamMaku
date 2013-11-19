open Definitions
open Util

let clients = ref []
let updates = ref []
let clients_lock = Mutex.create()
let updates_lock = Mutex.create()

let cUPDATE_SEPARATOR = "#"
let cARGUMENT_SEPARATOR = "$"
let cPOINT_SEPARATOR = "@"

let string_of_point (x,y) =
  (string_of_int (int_of_float x)) ^ cPOINT_SEPARATOR ^
  (string_of_int (int_of_float y))

let string_of_tile (x, y) =
  (string_of_int x) ^ cPOINT_SEPARATOR ^ (string_of_int y)

let combine_args = String.concat cARGUMENT_SEPARATOR

let string_of_update update =
  let soi = string_of_int in
  let sop = string_of_point in
  let soc = string_of_color in
  let sor = string_of_result in
  match update with
    | InitGraphics (args) ->
        combine_args ("InitGraphics"::(List.map soi args))
    | AddPlayer (id, col, pos) ->
        combine_args ["AddPlayer"; soi id; soc col; sop pos]
    | AddBullet (id, col, typ, pos) ->
        combine_args ["AddBullet"; soi id; soc col; 
        string_of_bullet_type typ; sop pos]
    | AddUFO (id, pos) -> combine_args ["AddUFO"; soi id; sop pos]
    | Graze -> "Graze"
    | MovePlayer (id, pos) -> combine_args ["MovePlayer"; soi id; sop pos]
    | MoveBullet (id, pos) -> combine_args ["MoveBullet"; soi id; sop pos]
    | MoveUFO (id, pos) -> combine_args ["MoveUFO"; soi id; sop pos]
    | DeleteBullet (id) -> combine_args ["DeleteBullet"; soi id]
    | DeleteUFO (id) -> combine_args ["DeleteUFO"; soi id]
    | UseBomb (color) -> combine_args ["UseBomb"; soc color]
    | SetBombs (color, i) -> combine_args ["SetBombs"; soc color; soi i]
    | SetLives (color, i) -> combine_args ["SetLives"; soc color; soi i]
    | SetScore (col, i) -> combine_args ["SetScore"; soc col; soi i]
    | SetPower (col, i) -> combine_args ["SetPower"; soc col; soi i]
    | SetCharge (col, i) -> combine_args ["SetCharge"; soc col; soi i]
    | Countdown (i) -> combine_args ["Countdown"; soi i]
    | GameOver (res) -> combine_args ["GameOver"; sor res]

let parse_updates updates =
  Mutex.lock updates_lock;
  let string_fold acc update =
    (* let _ = print_endline (string_of_update update) in*)
    acc ^ string_of_update update ^ cUPDATE_SEPARATOR in
  let sendable = List.fold_left string_fold "" updates in
  Mutex.unlock updates_lock; sendable

let init_server port =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server Unix.SO_REUSEADDR true;
  Unix.setsockopt server Unix.SO_KEEPALIVE false;
  Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen server 100;
  server

let add_clients server =
  while true do
    let (c, a) = Unix.accept server in
    Mutex.lock clients_lock;
    print_endline "A client connected to gui server";
    clients := (Connection.server a c)::!clients;
    Mutex.unlock clients_lock;
  done

let init_single_connection port =
  let server = init_server port in
  let (c, a) = Unix.accept server in
  Mutex.lock clients_lock;
  print_endline "A client connected to gui server";
  clients := (Connection.server a c)::!clients;
  Mutex.unlock clients_lock;
  ignore(Thread.create add_clients server)

let init port = ignore(Thread.create add_clients (init_server port))

let add_update u =
  Mutex.lock updates_lock;
  updates := u::(!updates);
  Mutex.unlock updates_lock

let send u =
  Mutex.lock clients_lock;
  let parsed_updates = parse_updates u in
  clients := List.fold_left
               (fun new_clients c ->
                  if Connection.output_string c parsed_updates then
                    c::new_clients
                  else (new_clients)) [] !clients;
  Mutex.unlock clients_lock

let send_update u = send [u]

let send_updates() =
  Mutex.lock updates_lock;
  let u = List.rev !updates in
  updates := [];
  Mutex.unlock updates_lock;
  send u

