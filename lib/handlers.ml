open Syntax

module Helper : sig
  val redirect_or_error_response : 
    Dream.request ->
    path:string ->
    (_, [< Error.t]) result ->
    Dream.response Lwt.t
  
  val view_or_error_to_response : 
    (string, [< Error.t]) result -> Dream.response Lwt.t

  val look_up_get_opt_param : 
    Dream.request -> 
    string -> 
    string option 

  val look_up_post_param : 
    Dream.request -> 
    string -> 
    (string, [> Error.param]) Lwt_result.t

  val look_up_param :
    Dream.request ->
    string ->
    (string, [> Error.param]) Lwt_result.t
end = struct
  let redirect_or_error_response request ~path = function
    | Ok _ -> Dream.redirect request path
    | Error err -> 
        Dream.error (fun log -> log "%a" Error.pp err);
        Dream.html @@ Views.render_error 
          ~msg:(Format.asprintf "%a" Error.pp err)

  let view_or_error_to_response = function
    | Ok view -> Dream.html view
    | Error err -> 
        Dream.error (fun log -> log "%a" Error.pp err);
        Dream.html @@ Views.render_error 
          ~msg:(Format.asprintf "%a" Error.pp err)

  let look_up_get_opt_param request key =
    let uri = Dream.target request |> Uri.of_string in
    match Uri.get_query_param uri key with 
    | Some value when value <> String.empty -> Some value
    | _ -> None 

  let look_up_post_param request key =
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

  let look_up_param request key =
    try 
      Lwt_result.return (Dream.param request key)
    with Failure _ -> 
      Lwt_result.fail (`Key_not_found key)
end

let handle_rounds_list request =
  let view = 
    let ctx = Context.retrieve request in
    let+ queue = Rounds_queue.update ctx.queue in
    ctx.queue <- queue;
    let is_running = Rounds_queue.is_running queue in
    let rounds = Rounds_queue.to_list queue in
    let provers = Models.Prover.readdir ~dir:Options.binaries_dir in
    Ok (Views.render_rounds_list request ~is_running rounds provers) 
  in
  Lwt.bind view Helper.view_or_error_to_response

let handle_round_detail request =
  let view = 
    let ctx = Context.retrieve request in
    let name = Helper.look_up_get_opt_param request "name" in
    let res = Option.bind
      (Helper.look_up_get_opt_param request "res") 
      Models.Fields.Res.of_string
    in
    let expected_res = Option.bind
      (Helper.look_up_get_opt_param request "expected_res")
      Models.Fields.Res.of_string
    in
    let errcode = Option.bind
      (Helper.look_up_get_opt_param request "errcode")
      Models.Fields.Errcode.of_string
    in
    let only_diff = 
      Helper.look_up_get_opt_param request "only_diff"
      |> Option.is_some
    in
    Helper.look_up_param request "uuid"
    >>? Rounds_queue.find_by_uuid ctx.queue
    >>? Round.problems ?name ?res ?expected_res ?errcode ~only_diff
    >|? Views.render_round_detail request
  in
  Lwt.bind view Helper.view_or_error_to_response

let handle_problem_trace request = 
  let view =
    let ctx = Context.retrieve request in
    let*? uuid = Helper.look_up_param request "uuid"
    and*? name = 
      let*? name = Helper.look_up_param request "problem" in
      Lwt.return @@ Misc.from_base64url name
    in
    Rounds_queue.find_by_uuid ctx.queue uuid
    >>? Round.problem ~name
    >|? Views.render_problem_trace request
  in
  Lwt.bind view Helper.view_or_error_to_response

let pp_bp_config ~binary fmt =
  let binary_path = Filename.concat Options.binaries_dir binary in
  Format.fprintf fmt "\
  @[<v 1>(prover@ \
    @[<v 1>(name ae-read-status)@ \
      @[<v 1>(cmd \"grep :status $file\")@ \
        @[<v 1>(unknown \":status unknown\")@ \
          @[<v 1>(sat \":status sat\")@ \
            @[<v 1>(unsat \":status valid\"))@]@]@]@]@]@]@\n\
  @[<v 1>(dir@ \
    @[<v 1>(path \"%s\")@ \
      @[<v 1>(pattern \".*.ae|.*.smt2\")@ \
        @[<v 1>(expect (run ae-read-status)))@]@]@]@]@\n\
  @[<v 1>(prover@ \
    @[<v 1>(name alt-ergo)@ \
      @[<v 1>(cmd \"%s $file\")@ \
        @[<v 1>(sat \"^sat\")@ \
          @[<v 1>(unsat \"Valid|(^unsat)\")@ \
            @[<v 1>(unknown \"(I Don't Know)|(^unsat)\"))@]@]@]@]@]@]@\n"
  Options.tests_dir
  binary_path

let generate_bp_config ~binary =
  let filename, ch = 
    Filename.open_temp_file "benchtop_" "_config" 
  in
  let fmt = Format.formatter_of_out_channel ch in
  pp_bp_config ~binary fmt;
  close_out ch;
  filename

let handle_schedule_round request = 
  let ctx = Context.retrieve request in
  (Helper.look_up_post_param request "prover" 
  >|? fun prover ->
  let config_path = generate_bp_config ~binary:prover in
  let new_round = Round.make ~cmd:
    ("benchpress", [|
      "benchpress"
    ; "run"
    ; "-c"
    ; config_path
    ; "-p" 
    ; "alt-ergo" 
    ; "lib/tests"|]) 
    ~config:"default" 
  in
  Ok (ctx.queue <- Rounds_queue.push new_round ctx.queue))
  >>= Helper.redirect_or_error_response request ~path:"/"

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

let handle_rounds_diff request = 
  let ctx = Context.retrieve request in
  let get_db_file uuid =
    Helper.look_up_param request uuid
    >>? Rounds_queue.find_by_uuid ctx.queue
    >>? Round.db_file
  in
  Helper.view_or_error_to_response =<<
  let*? db_file1 = get_db_file "uuid1"
  and*? db_file2 = get_db_file "uuid2" in
  Actions.compare db_file1 db_file2
  >|? Views.render_rounds_diff request

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
