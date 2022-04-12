open! Base
open! Snake_lib

let height = 10
let width = 10

let test locations =
  Random.init 42;
  let board = Board.create ~height ~width in
  let locations = List.map locations ~f:(fun (row, col) -> Position.{ row; col }) in
  let snake = Snake.Exercises.create_of_locations locations in
  let apple = Apple.Exercises.exercise05 ~board ~snake in
  Stdio.printf !"%{sexp: Position.t option}\n%!" (Option.map ~f:Apple.location apple)
;;

let%expect_test "simple" =
  test [ 0, 1; 0, 2; 0, 3 ];
  [%expect {| (((col 9) (row 2))) |}]
;;

let%expect_test "only one valid location" =
  let locations = List.cartesian_product (List.range 0 height) (List.range 0 width) in
  let snake_locations =
    List.filter locations ~f:(fun square ->
        not ([%compare.equal: int * int] (4, 5) square))
  in
  test snake_locations;
  [%expect {| (((col 5) (row 4))) |}]
;;

let%expect_test "no valid locations" =
  let snake_locations =
    List.cartesian_product (List.range 0 height) (List.range 0 width)
  in
  test snake_locations;
  [%expect {| () |}]
;;
