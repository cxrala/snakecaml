open! Base

type t =
  { (* [direction] represents the orientation of the snake's head. *)
    mutable direction : Direction.t
  ; (* [extensions_remaining] represents how many more times we should extend the
       snake. *)
    mutable extensions_remaining : int
  ; (* [head] represents the current squares the snake head occupies. *)
    mutable head : Position.t
  ; (* [tail] represents the set of squares that the snake occupies, sorted in reverse
       order (i.e. the first element in the list represents the tip of the tail) *)
    mutable tail : Position.t list
  }
[@@deriving sexp_of, fields]

let to_string ?(indent = 0) { direction; extensions_remaining; head; tail } =
  Core.sprintf
    !{|Head position: %{Position}
Tail positions: [ %s ]
Direction: %{sexp: Direction.t}
Extensions remaining: %d|}
    head
    (List.map tail ~f:Position.to_string |> String.concat ~sep:"; ")
    direction
    extensions_remaining
  |> String.split_lines
  |> List.map ~f:(fun s -> String.init indent ~f:(fun _ -> ' ') ^ s)
  |> String.concat ~sep:"\n"
;;

let create ~length (direction, row, col) =
  { direction
  ; extensions_remaining = 0
  ; head = { Position.row; col }
  ; tail = List.init (length - 1) ~f:(fun c -> { Position.row; Position.col = c })
  }
;;

(* Exercise 06a *)
let grow_over_next_steps t by_how_much =
  t.extensions_remaining <- t.extensions_remaining + by_how_much
;;

let head t = t.head
let all_locations t = t.head :: t.tail
let set_direction t direction = t.direction <- direction

(* Exercise 01*)
let move_forward head tail direction =
  Direction.next_position direction head, List.tl_exn tail @ [ head ]
;;

(* Exercise 04a*)
let collides_with_self t = List.exists t.tail ~f:(Position.equal t.head)

(* Exercise 06c*)
let move_forward_and_grow ({ direction; extensions_remaining; head; tail } as t) =
  if extensions_remaining > 0
  then (
    t.extensions_remaining <- t.extensions_remaining - 1;
    Direction.next_position direction head, tail @ [ head ])
  else move_forward head tail direction
;;

let step t =
  let head, tail = move_forward_and_grow t in
  t.head <- head;
  t.tail <- tail;
  not (collides_with_self t)
;;

module Exercises = struct
  let exercise01 = move_forward

  let create_of_locations locations =
    let head = List.hd_exn locations in
    let tail = List.tl_exn locations |> List.rev in
    { direction = Right; head; tail; extensions_remaining = 0 }
  ;;

  let set_head snake head = snake.head <- head
  let set_tail snake tail = snake.tail <- tail
  let exercise04a = collides_with_self
  let exercise06a = grow_over_next_steps
  let exercise06c = move_forward_and_grow
  let exercise06c2 = step
end
