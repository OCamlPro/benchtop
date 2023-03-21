open Syntax

type t = { queue : Rounds_queue.t }

let ctx : t ref = ref { queue = Rounds_queue.empty }

let init () =
  let* queue = Rounds_queue.make ~dir:Options.db_dir in
  ctx := { queue };
  Lwt.return ()

let set c = ctx := c
let get () = !ctx

let update () =
  let+ queue = Rounds_queue.update !ctx.queue in
  ctx := { queue }
