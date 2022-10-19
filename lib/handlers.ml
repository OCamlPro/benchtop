open Syntax

module Helper : sig
  val view_to_response : (string, Error.t) result -> Dream.response Lwt.t

  val look_up_get_param : 
    string -> 
    Dream.request -> 
    string option

  val look_up_post_param : 
    string -> 
    Dream.request -> 
    (string, Error.t) Lwt_result.t
end = struct
  let view_to_response = function
    | Ok view -> Dream.html view
    | Error err -> 
        Dream.error (fun log -> log "%a" Error.pp err);
        Dream.html @@ Views.render_error 
          ~msg:(Format.asprintf "%a" Error.pp err)

  let look_up_get_param key request =
    let uri = Dream.target request |> Uri.of_string in
    match Uri.get_query_param uri key with 
    | Some value when value <> String.empty -> Some value
    | _ -> None

  let look_up_post_param key request =
    Dream.form request >|= function
    | `Ok params -> 
        List.assoc_opt key params 
        |> Option.to_result ~none:(`Key_not_found key) 
    | `Expired x -> Error (`Expired x)
    | `Wrong_session x -> Error (`Wrong_session x)
    | `Invalid_token x -> Error (`Invalid_token x)
    | `Missing_token x -> Error (`Missing_token x)
    | `Many_tokens x -> Error (`Many_tokens x)
    | `Wrong_content_type -> Error (`Wrong_content_type)
end

let handle_rounds_list request = (
  let ctx = Context.retrieve request in
  let+? queue = Rounds_queue.update ctx.queue in
  ctx.queue <- queue;
  let queue = Rounds_queue.to_list queue
  and is_running = Rounds_queue.is_running queue in
  Views.render_rounds_list request ~is_running queue) 
  >>= Helper.view_to_response


let handle_round_detail request = (
  let ctx = Context.retrieve request in
  let uuid = Dream.param request "uuid" in
  let*? round = Rounds_queue.find_by_uuid uuid ctx.queue in
  let name = Helper.look_up_get_param "name" request in
  let (>>) f g = Option.bind f g in
  let res = 
    Helper.look_up_get_param "res" request 
    >> Models.Fields.Res.of_string
  in
  let expected_res = 
    Helper.look_up_get_param "expected_res" request
    >> Models.Fields.Res.of_string
  in
  let errcode = 
    Helper.look_up_get_param "errcode" request
    >> Models.Fields.Errcode.of_string
  in
  let only_diff = 
    Helper.look_up_get_param "only_diff" request
    |> Option.is_some
  in
  let+? pbs = 
    Round.problems ?name ?res ?expected_res ?errcode ~only_diff round 
  in
  Views.render_round_detail request pbs)
  >>= Helper.view_to_response

let handle_problem_trace request = (
  let ctx = Context.retrieve request in
  let uuid = Dream.param request "uuid" in
  let name = Dream.param request "problem" 
    |> Dream.from_base64url |> Option.to_result ~none:`Not_found 
  in
  let*? round = Rounds_queue.find_by_uuid uuid ctx.queue 
  and*? name = Lwt.return name in 
  let+? pb = Round.problem ~name round in
  Views.render_problem_trace request pb)
  >>= Helper.view_to_response

let handle_schedule_round request = 
  let ctx = Context.retrieve request in
  let new_round = Round.make ~cmd:
    ("benchpress", [|"benchpress"; "run"; "-c";
      "lib/config.sexp"; "-p"; "alt-ergo"; "lib/tests"|]) ~config:"default" in
  ctx.queue <- Rounds_queue.push new_round ctx.queue;
  Dream.form request >>= function
  | `Ok _ -> Dream.redirect request "/"
  | _ -> Dream.empty `Bad_Request

let handle_stop_round request =
  Dream.form request >>= function
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

  let compare db_file1 db_file2 =
    Models.(retrieve ~db_file:db_file1 ~db_attached:db_file2 
      (Problem_diff.select ()))
end

let handle_rounds_diff request = (
  let ctx = Context.retrieve request in
  let uuid1 = Dream.param request "uuid1" in
  let uuid2 = Dream.param request "uuid2" in
  let*? round1 = Rounds_queue.find_by_uuid uuid1 ctx.queue
  and*? round2 = Rounds_queue.find_by_uuid uuid2 ctx.queue in
  let*? db_file1 = Lwt.return @@ Round.db_file round1 
  and*? db_file2 = Lwt.return @@ Round.db_file round2 in
  let+? pb_diffs = Actions.compare db_file1 db_file2 in
  Views.render_rounds_diff request pb_diffs)
  >>= Helper.view_to_response

(* TODO: Clean up *)
let handle_round_action_dispatcher request =
  Dream.form request >>= function
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

let handle_problems_list _request = 
  Dream.empty `Bad_Request
