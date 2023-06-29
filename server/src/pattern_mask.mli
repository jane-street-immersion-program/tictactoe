open! Core
open Tic_tac_toe_2023_common
open Protocol

type t

val horizontal : size:int -> score:float -> t
val vertical : size:int -> score:float -> t
val bearish : size:int -> score:float -> t
val bullish : size:int -> score:float -> t

module Board : sig
  type t

  val of_pieces : Piece.t Position.Map.t -> t
end
