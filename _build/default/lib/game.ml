open! Base

module Snake_state = struct
  type t =
    { snake_state : Snake.t
    ; mutable score : int
    }
  [@@deriving sexp_of]

  let to_string { snake_state; score } =
    Core.sprintf "%s (%d)" (Snake.to_string ~indent:2 snake_state) score
  ;;
end

type t =
  { mutable snake : Snake_state.t
  ; mutable snake2 : Snake_state.t
  ; mutable game_state : Game_state.t
  ; mutable apple : Apple.t
  ; board : Board.t
  }
[@@deriving sexp_of]

let to_string { snake; snake2; game_state; apple; board } =
  Core.sprintf
    !{|Game state: %{sexp:Game_state.t}
Apple: %{sexp:Apple.t}
Board: %{sexp:Board.t}
Snake 1:
%s
Snake 2:
%s 
|}
    game_state
    apple
    board
    (Snake_state.to_string snake)
    (Snake_state.to_string snake2)
;;

let create ~height ~width ~initial_snake_length =
  let board = Board.create ~height ~width in
  let snake =
    Snake.create ~length:initial_snake_length (Direction.Right, 0, initial_snake_length)
  in
  let snake2 =
    Snake.create
      ~length:initial_snake_length
      (Direction.Right, height - 1, initial_snake_length)
  in
  let apple = Apple.create ~board ~snake:[ snake; snake2 ] in
  let snake_state1 = { Snake_state.snake_state = snake; score = 0 } in
  let snake_state2 = { Snake_state.snake_state = snake2; score = 0 } in
  match apple with
  | None -> failwith "unable to create initial apple"
  | Some apple ->
    let t =
      { snake = snake_state1
      ; snake2 = snake_state2
      ; apple
      ; game_state = In_progress
      ; board
      }
    in
    if List.exists (Snake.all_locations snake) ~f:(fun pos ->
           not (Board.in_bounds t.board pos))
       || List.exists (Snake.all_locations snake2) ~f:(fun pos ->
              not (Board.in_bounds t.board pos))
    then failwith "unable to create initial snake"
    else t
;;

let snake t = t.snake.snake_state
let snake2 t = t.snake2.snake_state
let apple t = t.apple
let game_state t = t.game_state
let score t = t.snake.score
let score2 t = t.snake2.score

(* Exercise 02b *)
let handle_key t key =
  match Direction.of_key key with
  | Some (x, b) ->
    if b
    then Snake.set_direction t.snake.snake_state x
    else Snake.set_direction t.snake2.snake_state x
  | None -> ()
;;

(* Exercise 03b *)
let check_for_collisions t =
  let f snake =
    if not (Board.in_bounds t.board (Snake.head snake))
    then t.game_state <- Game_over "Out of bounds!"
  in
  f t.snake.snake_state;
  f t.snake2.snake_state
;;

(* Exercise 06b*)
let maybe_consume_apple t =
  let apple_score =
    match Apple.color t.apple with
    | Red -> 1
    | Gold -> 2
  in
  let f (snake : Snake_state.t) b =
    snake.score <- snake.score + apple_score;
    Snake.grow_over_next_steps snake.snake_state (Apple.amount_to_grow t.apple);
    match Apple.create ~board:t.board ~snake:[ t.snake.snake_state; t.snake2.snake_state ] with
    | None -> t.game_state <- Win b
    | Some x -> t.apple <- x
  in
  if Position.equal (Snake.head t.snake.snake_state) (Apple.location t.apple)
  then f t.snake true
  else if Position.equal (Snake.head t.snake2.snake_state) (Apple.location t.apple)
  then f t.snake2 false
;;

(* Exercise 04b*)
let step t =
  let f snake =
    if Snake.step snake
    then (
      check_for_collisions t;
      maybe_consume_apple t)
    else t.game_state <- Game_over "Self collision!"
  in
  f t.snake.snake_state;
  f t.snake2.snake_state
;;
