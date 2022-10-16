type ctx = { 
  mutable queue: Rounds_queue.t
}

module Helper : sig
  val look_up_param : string -> Dream.request -> string option
end = struct
  let look_up_param param request =
    let uri = Dream.target request |> Uri.of_string in
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
      let name = Helper.look_up_param "name" request in
      let res = Option.bind
        (Helper.look_up_param "res" request)
        Models.Fields.Res.of_string
      in
      let expected_res = Option.bind
        (Helper.look_up_param "expected_res" request)
        Models.Fields.Res.of_string
      in
      let errcode = Option.bind
        (Helper.look_up_param "errcode" request)
        Models.Fields.Errcode.of_string
      in
      let only_diff = Option.is_some
        (Helper.look_up_param "only_diff" request)
      in
      match%lwt Round.problems ?name ?res ?expected_res ?errcode ~only_diff
        round with
      | Ok pbs -> Views.render_round_detail pbs request
      | Error _ -> Views.render_404_not_found request
      end
  | None -> Views.render_404_not_found request

let handle_problem_trace ctx request =
  let uuid = Dream.param request "uuid" in
  let%lwt {queue; _} = ctx in 
  let name = Dream.param request "problem" |> Dream.from_base64url in
  match (Rounds_queue.find_by_uuid uuid queue, name) with
  | Some round, Some name -> begin
    match%lwt Round.problem ~name round with
    | Ok pb -> Views.render_problem_trace pb request
    | Error _ -> Views.render_404_not_found request
    end
  | None, _ | _, None -> Views.render_404_not_found request

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
  let extract_selected_items =
    let regexp = Str.regexp "item_[0-9]+" in
    fun lst ->
    List.fold_left (fun acc (key, value) ->
      if Str.string_match regexp key 0 then
        (Dream.from_base64url value) :: acc 
      else acc
    ) [] lst

  let compare db_file1 db_file2 request =
    match%lwt Models.(retrieve ~db_file:db_file1 ~db_attached:db_file2 
      (Problem_diff.select ())) with
    | Ok pb_diffs -> Views.render_rounds_diff pb_diffs request 
    | Error err -> begin
        Dream.error (fun log -> log "ouuups: %a" Error.pp err);
        Dream.empty `Bad_Request
      end
end

let handle_rounds_diff ctx request =
  let uuid1 = Dream.param request "uuid1" in
  let uuid2 = Dream.param request "uuid2" in
  let%lwt {queue; _} = ctx in
  match Rounds_queue.(find_by_uuid uuid1 queue, find_by_uuid uuid2 queue) with
  | Some {status = Done {db_file = db_file1; _}; _}, 
    Some {status = Done {db_file = db_file2; _}; _} -> 
      Actions.compare db_file1 db_file2 request
  | _ -> Dream.empty `Bad_Request

let handle_round_action_dispatcher ctx request =
  match%lwt Dream.form request with
  | `Ok (("action_kind", action_kind)::params) -> begin
      match action_kind with
      | "compare" -> begin
          match Actions.extract_selected_items params with 
          | [Some uuid1; Some uuid2] ->
              let path = Format.sprintf "/round/%s/diff/%s" uuid1 uuid2 in
              Dream.redirect request path 
          | _ -> Dream.empty `Bad_Request
          end
      | _ -> begin
          Dream.error (fun log -> log "Unknown action %s" action_kind);
          Dream.redirect request "/"
        end
      end
  | `Ok _ -> Dream.redirect request "/"
  | _ -> Dream.empty `Bad_Request
