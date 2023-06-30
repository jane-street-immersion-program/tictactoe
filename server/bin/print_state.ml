open! Core
open Async
open Tic_tac_toe_2023_common
open Protocol
module Rpc_client = Tic_tac_toe_2023_rpc_client

let command =
  Command.async_or_error
    ~summary:
      "Send off an rpc to create and join a game and then randomly places \
       pieces using web socket rpc"
    (let%map_open.Command port =
       flag
         "port"
         (optional_with_default 8181 int)
         ~doc:"port on which to serve"
     and host =
       flag "host" (required string) ~doc:"The host to connect to"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind client =
         let uri =
           Uri.empty
           |> Fn.flip Uri.with_host (Some host)
           |> Fn.flip Uri.with_port (Some port)
         in
         Rpc_client.create ~uri
       in
       let%bind state = Rpc_client.show_all_games_with_two_players client in
       print_s [%sexp (state : Game_state.t Game_id.Map.t)];
       return ())
;;

let () = Command_unix.run command
