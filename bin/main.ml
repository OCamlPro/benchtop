open Benchtop
open Syntax

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
        | "application/x-www-form-urlencoded"::_ -> begin
          Dream.form request >|= function
          | `Ok form_fields ->
              Dream.debug
                (fun log -> log "POST fields:@,%a" pp_fields form_fields)
          | _ -> ()
          end
        | _ -> Lwt.return ()
        end
    | None -> Lwt.return ()
  in
  inner_handler request

let rec update_context () =
  let* _ = Lwt_unix.sleep 1. in
  let* _ = Context.update () in
  update_context ()

let server log_level interface port=
  Dream.(serve ~interface ~port
  @@ logger
  @@ memory_sessions
  @@ header_logger
  @@ flash
  @@ router [
      get "/css/**" @@ static (List.hd Location.Sites.css)
    ; get "/scripts/**" @@ static (List.hd Location.Sites.scripts)
    ; get "/" @@ Handlers.handle_rounds_list
    ; scope "/round" [origin_referrer_check] [
        get "/:uuid" @@ Handlers.handle_round_detail
      ; get "/:uuid/problem/:problem"
        @@ Handlers.handle_problem_trace
      ; get "/:uuid1/diff/:uuid2" @@ Handlers.handle_rounds_diff
      ; post "/action" @@ Handlers.handle_round_action_dispatcher
      ]
    ; scope "/benchpress" [origin_referrer_check] [
        post "/schedule" @@ Handlers.handle_schedule_round
      ; post "/stop" @@ Handlers.handle_stop_round
    ]
    ; scope "/problems" [origin_referrer_check] [
        get ":uuid" @@ Handlers.handle_problems_list
    ]
  ])

let main log_level interface port number_of_jobs share_dir =
  Dream.initialize_log ~level:log_level ();
  Options.set_number_of_jobs number_of_jobs;
  Options.set_share_dir share_dir;
  Lwt_main.run (
    let* _ = Context.init () in
    Lwt.async (update_context);
    server log_level interface port)

module Cmd = struct
  open Cmdliner

  let log_level =
    let doc = "Set the logging level." in
    let level = Arg.enum [
        "info", `Info
      ; "warning", `Warning
      ; "error", `Error
      ; "debug", `Debug
    ] in
    Arg.(value & opt level `Info
      & info ["l"; "log"] ~docv:"LOG_LEVEL" ~doc)

  let interface =
    let doc = "Specify the listen address." in
    Arg.(value & opt string "localhost"
      & info ["i"; "interface"] ~docv:"INTERFACE" ~doc)

  let port =
    let doc = "Specify the listen port." in
    Arg.(value & opt int 8080
      & info ["p"; "port"] ~docv:"PORT" ~doc)

  let number_of_jobs =
    let doc = "Set the number of jobs." in
    Arg.(value & opt int Options.number_of_jobs
      & info ["j"] ~docv:"NUMBER" ~doc)

  let share_dir =
    let doc = "Set the share dir." in
    Arg.(value & opt string Options.share_dir
      & info ["d"] ~docv:"SHARE" ~doc)

  let cmd =
    let open Cmdliner in
    let doc = "A web interface for Benchpress" in
    let man = [
        `S Manpage.s_description
      ; `P "$(tname) runs a web interface for benchpress."
      ; `S Manpage.s_bugs; `P "Bug reports to <pierre.villemot@ocamlpro.com>"
    ] in
    let info = Cmd.info "benchtop" ~version:"dev" ~doc ~man in
    Cmd.v info Term.(const main $ log_level $ interface $ port
      $ number_of_jobs $ share_dir)

  let main () = exit (Cmd.eval cmd)
end

let () = Cmd.main ()

