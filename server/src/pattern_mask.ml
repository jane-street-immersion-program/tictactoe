open! Core
open Tic_tac_toe_2023_common
open Protocol

module Z = struct
  include Z

  let to_bits_hum z =
    let numbits = numbits z in
    match numbits with
    | 0 -> "0"
    | numbits ->
      List.init numbits ~f:(fun index ->
        match Z.testbit z index with true -> "1" | false -> "0")
      |> List.rev
      |> String.concat ~sep:""
  ;;

  let sexp_of_t z = Sexp.Atom (to_bits_hum z)
end

type t =
  { mask : Z.t
  ; width : int
  ; height : int
  ; score : float
  }
[@@deriving sexp_of]

let horizontal ~size ~score =
  let mask = Z.( + ) (Z.shift_left Z.one size) Z.minus_one in
  let height = 1 in
  let width = size in
  { mask; height; width; score }
;;

let vertical ~size ~score =
  let mask =
    List.init size ~f:(fun index ->
      Fn.apply_n_times ~n:index (fun z -> Z.shift_left z 15) Z.one)
    |> List.reduce_exn ~f:Z.logor
  in
  let height = size in
  let width = 1 in
  { mask; height; width; score }
;;

let bearish ~size ~score =
  let mask =
    List.init size ~f:(fun index ->
      Fn.apply_n_times ~n:index (fun z -> Z.shift_left z (15 + 1)) Z.one)
    |> List.reduce_exn ~f:Z.logor
  in
  let height = size in
  let width = size in
  { mask; height; width; score }
;;

let bullish ~size ~score =
  let start = Z.shift_left Z.one 4 in
  let mask =
    List.init size ~f:(fun index ->
      Fn.apply_n_times ~n:index (fun z -> Z.shift_left z (15 - 1)) start)
    |> List.reduce_exn ~f:Z.logor
  in
  let height = size in
  let width = size in
  { mask; height; width; score }
;;

let%test_module "mask tests" =
  (module struct
    let test mask = print_s [%sexp (mask : t)]

    let%expect_test "horizontal" =
      test (horizontal ~size:5 ~score:4.0);
      [%expect {| ((mask 11111) (width 5) (height 1) (score 4)) |}]
    ;;

    let%expect_test "vertical" =
      test (vertical ~size:5 ~score:4.0);
      [%expect
        {|
        ((mask 1000000000000001000000000000001000000000000001000000000000001)
         (width 1) (height 5) (score 4)) |}]
    ;;

    let%expect_test "bearish" =
      test (bearish ~size:5 ~score:4.0);
      [%expect
        {|
        ((mask 10000000000000001000000000000000100000000000000010000000000000001)
         (width 5) (height 5) (score 4)) |}]
    ;;

    let%expect_test "bullish" =
      test (bullish ~size:5 ~score:4.0);
      [%expect
        {|
        ((mask 1000000000000010000000000000100000000000001000000000000010000)
         (width 5) (height 5) (score 4)) |}]
    ;;
  end)
;;

module Board = struct
  type t =
    { x : Z.t
    ; o : Z.t
    }
  [@@deriving sexp_of]

  let index_of ~row ~column = (row * 15) + column

  let of_pieces (pieces : Piece.t Position.Map.t) : t =
    Map.fold
      pieces
      ~init:{ x = Z.zero; o = Z.zero }
      ~f:(fun ~key:{ row; column } ~data acc ->
      match data with
      | X ->
        { acc with
          x = Z.logor acc.x (Z.shift_left Z.one (index_of ~row ~column))
        }
      | O ->
        { acc with
          o = Z.logor acc.o (Z.shift_left Z.one (index_of ~row ~column))
        })
  ;;
end

let%test_module "board tests" =
  (module struct
    let%expect_test "simple board" =
      let board =
        [ Piece.X, 0, 0;
        O, 2, 0;
        X, 1, 1;
        O, 2, 1;
        X, 2, 2 ]
        |> List.map ~f:(fun (piece, row, column) ->
             { Position.row; column }, piece)
        |> Position.Map.of_alist_exn
        |> Board.of_pieces
      in
      print_s [%sexp (board : Board.t)];
      [%expect {| ((x 100000000000000010000000000000001) (o 11000000000000000000000000000000)) |}]
    ;;
  end)
;;
