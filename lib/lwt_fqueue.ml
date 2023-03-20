open Syntax

type 'a t = 'a list * 'a list * 'a list

let empty = ([], [], [])
let push x (hist, pop, push) = (hist, pop, x :: push)

let rec apply_map f = function
  | _, [], [] -> None
  | hist, x :: pop, push ->
      let y = f x in
      Some (y, (y :: hist, pop, push))
  | hist, [], push -> apply_map f (hist, List.rev push, [])

let pop queue = apply_map (fun x -> x) queue
let take queue = Option.map fst (pop queue)

let replace y queue =
  apply_map (fun _ -> y) queue |> Option.map snd |> Option.value ~default:queue

let of_list lst = (lst, [], [])
let to_list (hist, pop, push) = List.rev_append hist (List.rev_append push pop)

let pp =
  let pp_sep fmt () = Format.fprintf fmt ", @," in
  fun pp_el fmt queue ->
    to_list queue |> Format.(pp_print_list ~pp_sep pp_el fmt)
