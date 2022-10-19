open Syntax

type t = {
  mutable queue: Rounds_queue.t;
}

let field = Dream.new_field ()

let init () = Lwt_main.run (
  let* queue = Rounds_queue.make ~dir:Options.benchpress_share_dir in
  Lwt.return {queue})

let middleware ctx inner_handler request =
  Dream.set_field request field ctx; 
  inner_handler request


let retrieve request = 
  match Dream.field request field with
  | Some ctx -> ctx
  | None -> failwith "No context found"

