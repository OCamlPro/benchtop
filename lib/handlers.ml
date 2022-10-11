type ctx = { 
  mutable queue: Rounds_queue.t
}

let handle_rounds_list ctx request =
  let%lwt ({queue; _} as ctx) = ctx in 
  let%lwt queue = Rounds_queue.update queue in
  ctx.queue <- queue;
  Views.render_rounds_list (Rounds_queue.to_list queue) request
  |> Dream.html

let handle_round_detail ctx request = 
  let uuid = Dream.param request "uuid" in
  let%lwt {queue; _} = ctx in 
  match Rounds_queue.find_by_uuid uuid queue with
  | Some round -> begin
      match%lwt Round.problems round with
      | Ok pbs ->
          Views.render_round_detail pbs request |> Dream.html 
      | Error _ -> 
          Views.render_404_not_found request |> Dream.html
      end
  | None -> 
      Views.render_404_not_found request |> Dream.html

let handle_problem_trace ctx request =
  let uuid = Dream.param request "uuid" in
  let%lwt {queue; _} = ctx in 
  let name = Dream.param request "problem" |> Dream.from_base64url in
  match (Rounds_queue.find_by_uuid uuid queue, name) with
  | Some round, Some name -> begin
    match%lwt Round.problem round name with
    | Ok pb ->
      Views.render_problem_trace pb request |> Dream.html
    | Error _ -> 
      Views.render_404_not_found request |> Dream.html
    end
  | None, _ | _, None -> 
      Views.render_404_not_found request |> Dream.html

let handle_schedule_round ctx request =
  let%lwt ({queue; _} as ctx) = ctx in 
  let new_round = Round.make ~cmd:
    ("benchpress", [|"benchpress"; "run"; "-c"; 
      "lib/config.sexp"; "-p"; "alt-ergo"; "lib/tests"|]) ~config:"default" in
  ctx.queue <- Rounds_queue.push new_round queue;
  match%lwt Dream.form request with 
  | `Ok _ -> Dream.redirect request "/"
  | _ -> Dream.empty `Bad_Request

let handle_stop_round ctx request = 
  match%lwt Dream.form request with 
  | `Ok _ -> Dream.redirect request "/"
  | _ -> Dream.empty `Bad_Request

let handle_round_action ctx request =
  match%lwt Dream.form request with
  | `Ok (("action_kind", action_kind)::tl) -> begin
      match action_kind with
      | "compare" -> begin
          Dream.log "%s" (Misc.sprintf_list (fun fmt (key, value) -> Format.fprintf fmt "key: %s, value: %s" key value) tl);
          Dream.redirect request "/"
        end
      | _ -> begin
          Dream.error (fun log -> log "Unknown action %s" action_kind);
          Dream.redirect request "/"
        end
      end
  | `Ok _ -> Dream.redirect request "/"
  | _ -> Dream.empty `Bad_Request
