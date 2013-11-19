open Definitions
open Constants
open Util

let host =
  try (Unix.gethostbyname ((Sys.argv).(1))).Unix.h_addr_list.(0)
  with _ -> failwith ("Must provide hostname of game server as " ^
                      "first command-line argument for team to " ^
                      "connect to.")
let port =
  try int_of_string ((Sys.argv).(2))
  with _ -> cDEFAULT_BOT_PORT_NUMBER
let addr = Unix.ADDR_INET (host, port)
let retries = 10
let conn = ref None
let mutex = Mutex.create ()
let _ = Random.self_init ()

(* Helper functions *)
let val_of x =
  match x with
  | Some y -> y
  | None -> failwith "Tried to val_of None"

(* Communication Stuff *)

(* Doesn't return until a connection is established. *)
let recreate_conn () =
  let conn' = ref None in
  while !conn' = None do
    conn' := Connection.init addr retries
  done; conn := !conn';
  match !conn with
  | Some conn' -> conn'
  | None -> failwith "Could not create connection in recreate_conn"

let get_conn () =
  match !conn with
  | Some conn -> conn
  | None -> recreate_conn ()

let set_conn c = conn := c

let wait_for_game_start () =
  let input = ref(Connection.input (get_conn ())) in
  while !input = None do
    input := Connection.input (get_conn ())
  done;
  match !input with
  | Some (Control(GameStart)) -> ()
  | _ -> failwith "Didn't receive GameStart in wait_for_game_start (team.ml)"

let send_action a =
  let conn = get_conn() in
  if Connection.output conn (Action(a)) then ()
  else failwith "Server didn't respond to action request."

let rec read_data receiver () =
  let conn = get_conn () in
  match Connection.input conn with
  | Some (Data d) -> (receiver d; Thread.delay cUPDATE_TIME;
    read_data receiver ())
  | Some (Control GameEnd) -> print_endline "Game has ended."
  | Some c -> (read_data receiver ())
  | None -> print_endline "Connection to server lost."

let start_bot bot receiver =
  conn := Connection.init addr 10;
  match !conn with
  | Some conn ->
      if Connection.output conn (Control(GameRequest)) then
        (match Connection.input conn with
        | Some (Control(Team(c))) ->
            print_endline ("Bot has color " ^ (string_of_color c));
            wait_for_game_start ();
            ignore (Thread.create (read_data receiver) ());
            ignore (bot c);
            Connection.close conn
        | _ -> failwith "Server didn't respond to game request with team.")
      else failwith "Unable to send value to game server."
  | None -> failwith "Can't connect to server."
