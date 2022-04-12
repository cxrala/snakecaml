open! Base

module Color = struct
  type t =
    | Red
    | Gold
  [@@deriving sexp_of]
end

type t =
  { location : Position.t
  ; color : Color.t
  }
[@@deriving sexp_of]

let location t = t.location
let color t = t.color

let amount_to_grow t =
  match color t with
  | Red -> 2
  | Gold -> 4
;;

(* Exercise 05 *)
let rec flatten acc snake_list =
  match snake_list with
  | [] -> acc
  | h :: t ->
    let acc = h @ acc in
    flatten acc t
;;

let create ~board ~snake =
  let diff bs ss =
    List.filter bs ~f:(fun x -> not (List.mem ss x ~equal:Position.equal))
  in
  let difference =
    diff (Board.all_locations board) (flatten [] (List.map snake ~f:Snake.all_locations))
  in
  match difference with
  | [] -> None
  | _ ->
    let random_pos = List.random_element_exn difference in
    Some
      { location = { Position.row = random_pos.row; col = random_pos.col }
      ; color =
          (if List.length (flatten [] (List.map snake ~f:Snake.tail)) % 8 = 0
          then Gold
          else Red)
      }
;;

module Exercises = struct
  let exercise05 = create
  let create_with_location location = { location; color = Red }
end
