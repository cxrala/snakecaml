open! Base

type t =
  | In_progress
  | Game_over of string
  | Win of bool
[@@deriving sexp_of, compare]

let to_string t =
  match t with
  | In_progress -> ""
  | Game_over x -> "Game over: " ^ x
  | Win true -> "Player 1... WIN!"
  | Win false -> "Player 2... WIN!"
;;
