open Constants
open Definitions
open Util
open Game

let num_players = cNUM_TEAMS

let valOf x = match x with Some(y) -> y | None -> failwith "Tried to valOf None"

let listen port numConnections =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server Unix.SO_REUSEADDR true;
  Unix.setsockopt server Unix.SO_KEEPALIVE false;
  Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen server numConnections;
  server

(* Generic thread safety stuff *)
let getTSValue (r: 'a ref) (m: Mutex.t) : 'a =
  Mutex.lock m;
  let v = !r in
  Mutex.unlock m;
  v

(* It turns out that the game used to crash when the GUI closed because 
 * the GUI connection would be closed, and we would be trying to flush
 * text through a broken pipe. So the entire game would be killed by a 
 * SIGPIPE signal. *)
let handle_error_signal code =
  print_endline ("Encountered error signal " ^ (string_of_int code) ^ 
    " (probably broken pipe from GUI crash)")

let startGameServer port guiPort =
  Sys.set_signal Sys.sigpipe (Sys.Signal_handle handle_error_signal);
  print_endline "Starting game.";
  let _ = Netgraphics.init guiPort in
  let numConnections = 100 in
  let server = listen port numConnections in
  let shutdown = ref false in
  let shutdownMutex = Mutex.create () in

  let red_player = ref None in
  let blue_player = ref None in

 (* [assign_color client color] attempts to notify a just-connected client
   * that it has been assigned the specified color. *)
  let assign_color client color =
    if Connection.output client (Control (Team(color))) then
      print_endline ("Assigned team to " ^ (string_of_color color))
    else
      failwith "Lost connection while trying to assign color." in

  (* [acceptPlayers ()] listens for connections from 2 bots, assigning
   * them the colors Red and Blue. It blocks until the 2 bots have connected. *)
  let rec acceptPlayers () =
    let _ = try
      let (c, a) = Unix.accept server in
      if getTSValue shutdown shutdownMutex then () else
      let client = Connection.server a c in
      match Connection.input client with
      | Some(Control(GameRequest)) ->
          let col = if !red_player = None then
            (red_player := Some client; Red)
          else
            (blue_player := Some client; Blue) in
          let () = print_endline "A client has connected." in
          assign_color client col
      | _ -> Connection.close client
    with e -> print_endline ("Listening for a client connection 
                          resulted in exception : " ^ Printexc.to_string e) in
    if !blue_player = None then acceptPlayers () else () in

  let rec countdown i =
    if i = 0 then Netgraphics.send_update (Countdown 0)
    else (print_endline (string_of_int i);
          Netgraphics.send_update (Countdown (i));
          Thread.delay 1.0;
          countdown (i-1)) in

  acceptPlayers();

  let const_list = [cBOARD_WIDTH; cBOARD_HEIGHT; cSPREAD_RADIUS; cTRAIL_RADIUS;
    cBUBBLE_RADIUS; cINITIAL_LIVES; cINITIAL_BOMBS] in
  Netgraphics.send_update (InitGraphics (const_list));

  let game = ref (init_game ()) in
  let game_mutex = Mutex.create () in
 
  countdown 3;

  print_endline "Starting game.";

  let red_p, blue_p = match !red_player, !blue_player with
    | Some(r), Some(c) -> r,c
    | _ -> failwith "Connections are none." in

  let rec handle_connection game col client =
    let () = match Connection.input client with
    | Some(Action(a)) ->
        (Mutex.lock game_mutex;
         game := handle_action !game col a;
         Mutex.unlock game_mutex)
    | Some(_) -> (print_endline ("Invalid command received from " ^
        (string_of_color col)))
    | None -> (failwith ("Lost connection to " ^
        (string_of_color col))) in
    handle_connection game col client in

  if Connection.output red_p (Control(GameStart))
    && Connection.output blue_p (Control(GameStart)) then
      print_endline "Game started."
  else failwith "Connections to one or more players lost when starting game."; 
  
  ignore (Thread.create (handle_connection game Red) red_p);
  ignore (Thread.create (handle_connection game Blue) blue_p);

  let update_players game =
    let d = Data (get_data !game) in
    if Connection.output red_p d && Connection.output blue_p d then ()
    else print_endline 
      "Connections to one or more players lost when sending data." in

  let rec run_game game =
    let (new_g, res) = handle_time !game in
    match res with
    | Unfinished ->
        (game := new_g;
        update_players game;
        Netgraphics.send_updates ();
        Thread.delay cUPDATE_TIME;
        run_game game)
    | anything_else -> anything_else in

  let res = run_game game in
  Netgraphics.send_updates ();
  Netgraphics.send_update (GameOver (res));
  let () = match res with
  | Winner(c) -> print_endline ((string_of_color c) ^ " wins!")
  | Tie -> print_endline "The game was a tie!"
  | _ -> failwith "Should not return Unfinished" in

  Connection.close red_p;
  Connection.close blue_p;
  print_endline "Closed connections."

let () =
  let port = try int_of_string ((Sys.argv).(1))
             with _ -> cDEFAULT_PORT_NUMBER in
  let guiPort = try int_of_string ((Sys.argv).(2))
                with _ -> cDEFAULT_GUI_PORT_NUMBER in
  startGameServer port guiPort
