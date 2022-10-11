open Benchtop

let header_logger inner_handler request =
  let fields = Dream.all_headers request in
  let pp_fields = Misc.pp_list (fun fmt (key, value) ->
    Format.fprintf fmt "%s: %s" key value
  ) in
  Dream.debug (fun log -> log "%a" pp_fields fields);
  let _ = 
    match Dream.header request "Content-type" with
    | Some content_type -> begin
        match String.split_on_char ';' content_type with
        | "application/x-www-form-urlencoded"::_ -> 
          let _ = match%lwt Dream.form request with
          | `Ok form_fields -> 
              Lwt.return @@ Dream.debug 
                (fun log -> log "%a" pp_fields form_fields)
          | _ -> Lwt.return ()
          in ()
        | _ -> ()
        end
    | None -> () 
  in
  inner_handler request

let () = 
  let ctx = 
    let%lwt queue = Rounds_queue.make ~dir:Options.benchpress_share_dir in
    Lwt.return ({queue} : Handlers.ctx)
  in
  Dream.initialize_log ~level:`Debug ();
  Dream.run 
  @@ Dream.logger
  @@ header_logger
  @@ Dream.memory_sessions
  @@ Dream.router [
      Dream.get "/css/**" @@ Dream.static (List.hd Location.Sites.css)
    ; Dream.get "/" @@ Handlers.handle_rounds_list ctx 
    ; Dream.scope "/round" [Dream.origin_referrer_check] [
        Dream.get "/:uuid" @@ Handlers.handle_round_detail ctx
      ; Dream.get "/:uuid/problem/:problem" 
        @@ Handlers.handle_problem_trace ctx 
      ; Dream.post "/action" @@ Handlers.handle_round_action ctx
      ]
    ; Dream.post "/schedule" @@ Handlers.handle_schedule_round ctx
    ; Dream.get "/stop" @@ Handlers.handle_stop_round ctx 
  ] 
