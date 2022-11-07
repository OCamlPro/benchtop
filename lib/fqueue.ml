open Syntax 

type 'a t = 'a list * 'a list * 'a list

let empty = ([], [], [])

let push x (hist, pop, push) = (hist, pop, x :: push)

let rec pop_map f = function
  | (_, [], []) as queue -> Lwt_result.return queue
  | (hist, x :: pop, push) ->
      f x >>? fun y -> Lwt_result.return (y :: hist, pop, push)
  | (hist, [], push) -> pop_map f (hist, List.rev push, [])

let rec take = function
  | (_, [], []) -> None
  | (_, x :: _, _) -> Some x
  | (hist, [], push) -> take (hist, List.rev push, [])

let of_list lst = (lst, [], [])

let to_list (hist, pop, push) =
  List.rev_append hist (List.rev_append push pop)

let pp =
  let pp_sep fmt () = Format.fprintf fmt ", @," in
  fun pp_el fmt queue ->
    to_list queue
    |> Format.(pp_print_list ~pp_sep pp_el fmt)
