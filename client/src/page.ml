open! Core
open Uri_parsing
open Tic_tac_toe_2023_common
open Protocol

module T = struct
  type t =
    | Homepage
    | Game of Game_id.t
  [@@deriving sexp, equal, typed_variants]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.end_of_path Parser.unit
    | Game -> Parser.from_path (Value_parser.stringable (module Game_id))
  ;;
end

include T

let non_versioned_parser = Parser.Variant.make (module T)
let parser = Versioned_parser.first_parser non_versioned_parser
