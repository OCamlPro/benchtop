open Benchtop

let header_logger inner_handler request =
  let fields = Dream.all_headers request in
  let pp_fields = Misc.pp_list (fun fmt (key, value) ->
    Format.fprintf fmt "%s: %s" key value
  ) in
  Dream.debug (fun log -> log "Http Header:@,%a" pp_fields fields);
  let _ = 
    match Dream.header request "Content-type" with
    | Some content_type -> begin
        match String.split_on_char ';' content_type with
        | "application/x-www-form-urlencoded"::_ -> 
          let _ = match%lwt Dream.form request with
          | `Ok form_fields ->
              Lwt.return @@ Dream.debug 
                (fun log -> log "POST fields:@,%a" pp_fields form_fields)
          | _ -> Lwt.return ()
          in ()
        | _ -> ()
        end
    | None -> () 
  in
  inner_handler request

let start_server interface port = 
  let interface = Option.value interface ~default:"localhost" in
  let port = Option.value port ~default:8080 in
  let ctx = 
    let%lwt queue = Rounds_queue.make ~dir:Options.benchpress_share_dir in
    Lwt.return ({queue} : Handlers.ctx)
  in
  Dream.initialize_log ~level:`Debug ();
  Dream.run ~interface ~port 
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ header_logger
  @@ Dream.router [
      Dream.get "/css/**" @@ Dream.static (List.hd Location.Sites.css)
    ; Dream.get "/" @@ Handlers.handle_rounds_list ctx 
    ; Dream.scope "/round" [Dream.origin_referrer_check] [
        Dream.get "/:uuid" @@ Handlers.handle_round_detail ctx
      ; Dream.get "/:uuid/problem/:problem" 
        @@ Handlers.handle_problem_trace ctx 
      ; Dream.post "/action" @@ Handlers.handle_round_action ctx
      ]
    ; Dream.scope "/benchpress" [Dream.origin_referrer_check] [
        Dream.post "/schedule" @@ Handlers.handle_schedule_round ctx
      ; Dream.post "/stop" @@ Handlers.handle_stop_round ctx
    ]
  ]

module Cmd = struct
  open Cmdliner

  let interface =
    let doc = "Specify the listen address." in
    Arg.(value & opt (some string) None & info ["i"; "interface"] ~doc)

  let port =
    let doc = "Specify the listen port." in
    Arg.(value & opt (some int) None & info ["p"; "port"] ~doc)

  let cmd =
    let open Cmdliner in
    let doc = "A web interface for Benchpress" in
    let man = [
        `S Manpage.s_description
      ; `P "$(tname) runs a web interface for benchpress."
      ; `S Manpage.s_bugs; `P "Bug reports to <pierre.villemot@ocamlpro.com>"
    ] in
    let info = Cmd.info "benchtop" ~version:"dev" ~doc ~man in
    Cmd.v info Term.(const start_server $ interface $ port)

  let main () = exit (Cmd.eval cmd)
end

let () = Cmd.main () 
  
