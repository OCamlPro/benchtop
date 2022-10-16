type ctx = { 
  mutable queue: Rounds_queue.t
}

module Helper : sig
  val get_query_param : Uri.t -> string -> string option
end = struct
  let get_query_param uri param =
    Option.bind (Uri.get_query_param uri param) (function
      | "" -> None
      | str -> Some str)
end

let handle_rounds_list ctx request =
  let%lwt ({queue; _} as ctx) = ctx in 
  let%lwt queue = Rounds_queue.update queue in
  ctx.queue <- queue;
  let is_running = Rounds_queue.is_running ctx.queue in
  Views.render_rounds_list ~is_running (Rounds_queue.to_list queue) request

let handle_round_detail ctx request =
  let uuid = Dream.param request "uuid" in
  let%lwt {queue; _} = ctx in
  match Rounds_queue.find_by_uuid uuid queue with
  | Some round -> begin
      let uri = Dream.target request |> Uri.of_string in
      let name = Helper.get_query_param uri "name" in
      let res = Option.bind
        (Helper.get_query_param uri "res")
        Models.Fields.Res.of_string
      in
      let expected_res = Option.bind
        (Helper.get_query_param uri "expected_res")
        Models.Fields.Res.of_string
      in
      let errcode = Option.bind
        (Helper.get_query_param uri "errcode")
        Models.Fields.Errcode.of_string
      in
      let only_diff = Option.is_some
        (Helper.get_query_param uri "only_diff")
      in
      match%lwt Round.problems ?name ?res ?expected_res ?errcode ~only_diff
        round with
      | Ok pbs ->
          Views.render_round_detail pbs request
      | Error _ ->
          Views.render_404_not_found request
      end
  | None ->
      Views.render_404_not_found request

let handle_problem_trace ctx request =
  let uuid = Dream.param request "uuid" in
  let%lwt {queue; _} = ctx in 
  let name = Dream.param request "problem" |> Dream.from_base64url in
  match (Rounds_queue.find_by_uuid uuid queue, name) with
  | Some round, Some name -> begin
    match%lwt Round.problem ~name round with
    | Ok pb ->
      Views.render_problem_trace pb request
    | Error _ -> 
      Views.render_404_not_found request
    end
  | None, _ | _, None -> 
      Views.render_404_not_found request

let handle_schedule_round ctx request =
  let%lwt ({queue; _} as ctx) = ctx in
  let new_round = Round.make ~cmd:
    ("benchpress", [|"benchpress"; "run"; "-c";
      "lib/config.sexp"; "-p"; "alt-ergo"; "lib/tests"|]) ~config:"default" in
  ctx.queue <- Rounds_queue.push new_round queue;
  match%lwt Dream.form request with
  | `Ok _ -> Dream.redirect request "/"
  | _ -> Dream.empty `Bad_Request

let handle_stop_round _ctx request =
  match%lwt Dream.form request with
  | `Ok _ -> Dream.redirect request "/"
  | _ -> Dream.empty `Bad_Request

module Actions = struct
  let compare =
    let extract_selected_items =
      let regexp = Str.regexp "item_[0-9]+" in
      fun lst ->
      List.fold_left (fun acc (key, value) ->
        if Str.string_match regexp key 0 then
          (Dream.from_base64url value) :: acc 
        else acc
      ) [] lst
    in
    fun fields request ->
    match extract_selected_items fields with
    | [Some db_file1; Some db_file2] -> begin
      (*let res = Sql.(exec ~db_file:db_file1 
        (compose (fun _ b -> b) (attach ~db_file:db_file2) compare)) 
        |> Sql.debug
      in*)
      let res = Lwt.return @@ Error "toto" in 
      match%lwt res with
      | Ok _ -> Dream.redirect request "/"
      | Error _ -> Dream.empty `Bad_Request
      end
    | _ -> 
        Dream.log "Wrong compare action";
        Dream.redirect request "/"
end

let handle_round_action _ctx request =
  match%lwt Dream.form request with
  | `Ok (("action_kind", action_kind)::params) -> begin
      match action_kind with
      | "compare" -> Actions.compare params request
      | _ -> begin
          Dream.error (fun log -> log "Unknown action %s" action_kind);
          Dream.redirect request "/"
        end
      end
  | `Ok _ -> Dream.redirect request "/"
  | _ -> Dream.empty `Bad_Request
