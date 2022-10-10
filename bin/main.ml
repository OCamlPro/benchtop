open Benchtop

let () = 
  let ctx = 
    let%lwt queue = Rounds_queue.make ~dir:Options.benchpress_share_dir in
    Lwt.return ({queue} : Handlers.ctx)
  in
  Dream.initialize_log ~level:`Debug ();
  Dream.run 
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [
      Dream.get "/css/**" @@ Dream.static (List.hd Location.Sites.css)
    ; Dream.get "/" @@ Handlers.handle_rounds_list ctx 
    ; Dream.get "/show/:uuid" @@ Handlers.handle_round_detail ctx
    (*; Dream.get "/show/:uuid/problem/:problem" @@ Handlers.handle_problem_trace *)
    ; Dream.post "/schedule" @@ Handlers.handle_schedule_round ctx
    ; Dream.get "/stop" @@ Handlers.handle_stop_round ctx 
  ] 
