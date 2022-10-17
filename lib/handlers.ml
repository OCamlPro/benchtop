open Lwt_result.Infix

type ctx = { 
  mutable queue: Rounds_queue.t
}

let (>|) : ('a, Error.t) Lwt_result.t -> 
  ('a -> string) -> Dream.response Lwt.t
  = fun res f -> 
  let g s = Lwt_result.return (f s) in
  match%lwt res >>= g with
  | Ok view -> Dream.html view
  | Error err -> 
      Dream.error (fun log -> log "%a" Error.pp err);
      Dream.html (Views.render_error 
        ~msg:(Format.asprintf "%a" Error.pp err))  

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
  Rounds_queue.update queue 
  >>= (fun queue -> 
    ctx.queue <- queue;
    Lwt_result.return Rounds_queue.(is_running queue, to_list queue))
    >| fun (is_running, queue) ->
      Views.render_rounds_list request ~is_running queue

let handle_round_detail ctx request =
  let uuid = Dream.param request "uuid" in
  let%lwt {queue; _} = ctx in
  Rounds_queue.find_by_uuid uuid queue 
  >>= (fun round ->
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
    Round.problems ?name ?res ?expected_res ?errcode ~only_diff round)
  >| Views.render_round_detail request

let handle_problem_trace ctx request =
  let uuid = Dream.param request "uuid" in
  let name = Lwt.return (Dream.param request "problem" 
    |> Dream.from_base64url |> Option.to_result ~none:`Not_found) 
  in
  let%lwt {queue; _} = ctx in 
  Lwt_result.both (Rounds_queue.find_by_uuid uuid queue) name 
  >>= (fun (round, name) -> 
    Round.problem ~name round)
  >| Views.render_problem_trace request 

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

  let compare db_file1 db_file2 =
    Models.(retrieve ~db_file:db_file1 ~db_attached:db_file2 
      (Problem_diff.select ()))
end

let handle_rounds_diff ctx request =
  let uuid1 = Dream.param request "uuid1" in
  let uuid2 = Dream.param request "uuid2" in
  let%lwt {queue; _} = ctx in
  (Lwt_result.both (Rounds_queue.find_by_uuid uuid1 queue) 
    (Rounds_queue.find_by_uuid uuid2 queue)
  >>= (fun (round1, round2) ->
    Lwt_result.both (Lwt.return @@ Round.db_file round1) 
      (Lwt.return @@ Round.db_file round2))
    >>= (fun (db_file1, db_file2) -> 
    Actions.compare db_file1 db_file2))
  >| Views.render_rounds_diff request

(* TODO: Clean up *)
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
