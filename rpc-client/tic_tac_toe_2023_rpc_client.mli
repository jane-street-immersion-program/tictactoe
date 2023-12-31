open! Core
open! Async
open! Tic_tac_toe_2023_common
open Protocol

type t

val create : uri:Uri.t -> t Deferred.Or_error.t

val create_game
  :  t
  -> query:Create_game.Query.t With_username.t
  -> Game_id.t Deferred.Or_error.t

val join_existing_game
  :  t
  -> query:Game_id.t With_username.t
  -> Join_existing_game.Response.t Deferred.Or_error.t

val show_all_games_with_two_players
  :  t
  -> Game_state.t Game_id.Map.t Deferred.Or_error.t

val get_game
  :  t
  -> query:Game_id.t
  -> Get_game.Response.t Deferred.Or_error.t

val list_all_joinable_games
  :  t
  -> Joinable_game.t Game_id.Map.t Deferred.Or_error.t

val take_turn
  :  t
  -> query:Take_turn.Query.t With_username.t
  -> Take_turn.Response.t Deferred.Or_error.t

val me : t -> Username.t Deferred.Or_error.t

type game_ai = me:Piece.t -> game_state:Game_state.t -> Position.t

val create_game_and_play
  :  t
  -> me:Username.t
  -> against:Difficulty.t option
  -> game_kind:Game_kind.t
  -> game_ai:game_ai
  -> refresh_rate:Time_float.Span.t
  -> unit Deferred.Or_error.t

val join_game_and_play
  :  t
  -> game_id:Game_id.t
  -> me:Username.t
  -> game_ai:game_ai
  -> refresh_rate:Time_float.Span.t
  -> unit Deferred.Or_error.t

val create_game_and_play_against_self
  :  t
  -> me:Username.t
  -> game_kind:Game_kind.t
  -> game_ai:game_ai
  -> refresh_rate:Time_float.Span.t
  -> unit Deferred.Or_error.t
