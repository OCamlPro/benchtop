open Syntax

let previous_page request = 
  match Dream.header request "Referer" with
  | Some url -> url
  | None -> "/"

(* TODO: Move this function somewhere else. *)
let rec map_opt f = function
  | [] -> []
  | hd::tl ->
      match f hd with
      | Some hd -> hd :: map_opt f tl
      | None -> []

module Helper : sig
  val redirect : 
    Dream.request ->
    (_, Error.t) result ->
    Dream.response Lwt.t
  
  val view_or_error_to_response :
    Dream.request ->
    (string, [< Error.t]) result -> Dream.response Lwt.t
end = struct 
  let redirect request = function
    | Ok _ ->
        previous_page request |> Dream.redirect request
    | Error err -> 
        Dream.error (fun log -> log "%a" Error.pp err);
        Error.set_session request err;
        previous_page request |> Dream.redirect request

  let view_or_error_to_response request = function
    | Ok view -> Dream.html view
    | Error err ->
        let err = (err :> Error.t) in
        Dream.error (fun log -> log "%a" Error.pp err);
        Error.set_session request err;
        previous_page request |> Dream.redirect request
end

let handle_rounds_list request =
  let view = 
    let ctx = Context.get () in
    let+ queue = Rounds_queue.update ctx.queue in
    Context.set {queue};
    let is_running = Rounds_queue.is_running queue in
    let rounds = Rounds_queue.to_list queue in
    let provers = Models.Prover.readdir ~dir:Options.binaries_dir in
    Ok (Views.render_rounds_list request ~is_running rounds provers) 
  in
  Lwt.bind view (Helper.view_or_error_to_response request)

let handle_round_detail request =
  let view = 
    let ctx = Context.get () in
    let file = Misc.look_up_get_opt_param request "file" in
    let res =
      Misc.look_up_get_params request "res"
      |> map_opt Models.Res.of_string
    in
    let file_expect =
      Misc.look_up_get_params request "file_expect"
      |> map_opt Models.Res.of_string
    in
    let errcode =
      Misc.look_up_get_params request "errcode"
      |> map_opt Models.Errcode.of_string
    in
    let only_diff =
      Misc.look_up_get_opt_param request "only_diff"
      |> Option.is_some
    in
    let page = Option.bind
      (Misc.look_up_get_opt_param request "page") 
      int_of_string_opt
      |> Option.value ~default:0 
    in
    let*? round = 
      Misc.look_up_param request "uuid"
      >>? Rounds_queue.find_by_uuid ctx.queue
    in
    let prover = Round.prover round in
    let*? summary = Round.summary round in
    let*? total = 
      Round.count ?file ~res ~file_expect ~errcode ~only_diff round 
    in 
    let+? pbs = 
      Round.problems ?file ~res ~file_expect ~errcode ~only_diff ~page round
    in
    Views.render_round_detail request ~page ~total ~prover summary pbs
  in
  view >>= Helper.view_or_error_to_response request

let handle_problem_trace request = 
  let view =
    let ctx = Context.get () in
    let*? uuid = Misc.look_up_param request "uuid"
    and*? name = 
      let*? name = Misc.look_up_param request "problem" in
      Lwt.return @@ Misc.from_base64url name
    in
    Rounds_queue.find_by_uuid ctx.queue uuid
    >>? Round.problem ~name
    >|? Views.render_problem_trace request
  in
  view >>= Helper.view_or_error_to_response request

let pp_bp_config ~(prover : Models.Prover.t) fmt () =
  let binary = Format.sprintf "%s-%s" prover.name prover.version in
  let binary_path = Filename.concat Options.binaries_dir binary in
  Format.fprintf fmt "\
  (import-prelude false)@\n\
  @[<v 2>(prover@ \
    @[<v 2>(name ae-read-status)@ \
      @[<v 2>(cmd \"zgrep :status $file\")@ \
        @[<v 2>(unknown \":status unknown\")@ \
          @[<v 2>(unknown \"\")@ \
            @[<v 2>(sat \":status sat\")@ \
              @[<v 2>(unsat \":status unsat|:status valid\"))@]@]@]@]@]@]@]@]@\n\
  @[<v 2>(dir@ \
    @[<v 2>(path \"%s\")@ \
      @[<v 2>(pattern \".*.ae|.*.smt2|.*.zip\")@ \
        @[<v 2>(expect (run ae-read-status)))@]@]@]@]@\n\
  @[<v 2>(prover@ \
    @[<v 2>(name %s)@ \
      @[<v 2>(version %s)@ \
        @[<v 2>(cmd \"%s --timelimit=$timeout $file\")@ \
          @[<v 2>(sat \"^sat\")@ \
            @[<v 2>(unsat \"Valid|(^unsat)\")@ \
              @[<v 2>(unknown \"(I Don't Know)|(^unsat)\")@ \
                @[<v 2>(timeout \"^timeout\"))@]@]@]@]@]@]@]@]@]@."
  Options.tests_dir
  prover.name
  prover.version
  binary_path

let generate_bp_config prover =
  let filename, ch =
    Filename.open_temp_file "benchpress_" ".sexp"
  in
  let fmt = Format.formatter_of_out_channel ch in
  pp_bp_config ~prover fmt ();
  Dream.debug (fun log -> log "%a" (pp_bp_config ~prover) ());
  close_out ch;
  filename

let handle_schedule_round request =
  let ctx = Context.get () in
  let action =
    let+? prover =
      Misc.look_up_post_param request "prover"
      >|? Yojson.Safe.from_string
      >>? fun p -> Lwt.return @@ Models.Prover.of_yojson p
      >|! fun err -> `Cannot_convert_json err
    in
    let config_path = generate_bp_config prover in
    let new_round = Round.make ~prover ~cmd:
      ("benchpress", [|
        "benchpress"
      ; "run"
      ; "-j"
      ; string_of_int Options.number_of_jobs
      ; "-c"
      ; config_path
      ; "-t"
      ; string_of_int Options.prover_timeout
      ; "-p" 
      ; "alt-ergo"
      ; Options.tests_dir|])
    in
    Ok ({queue=Rounds_queue.push new_round ctx.queue} |> Context.set)
  in
  action >>= Helper.redirect request

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
end

let handle_rounds_diff request = 
  let ctx = Context.get () in
  let file = 
    Misc.look_up_get_opt_param request "file"
    |> Option.value ~default:""
  in
  let kind_diff = Option.bind 
    (Misc.look_up_get_opt_param request "kind_diff")
    Models.Kind_diff.of_string
    |> Option.value ~default:Models.Kind_diff.Difference
  in
  let show_rtime_reg = 
    Misc.look_up_get_opt_param request "show_rtime_reg"
    |> Option.is_some
  in
  let page =
    Misc.look_up_get_opt_param request "page"
    |> fun x -> Option.bind x int_of_string_opt
    |> Option.value ~default:0
  in
  let view =
    let*? round1 = 
      Misc.look_up_param request "uuid1"
      >>? Rounds_queue.find_by_uuid ctx.queue
    and*? round2 = 
      Misc.look_up_param request "uuid2"
      >>? Rounds_queue.find_by_uuid ctx.queue
    in
    let*? db_file1 = Round.db_file round1 in
    let*? db_file2 = Round.db_file round2 in
    let*? total =
      Models.(retrieve ~db_file:db_file1 ~db_attached:db_file2 
        (Problem_diff.count ~file ~kind_diff ~show_rtime_reg))
    in 
    Models.(retrieve ~db_file:db_file1 ~db_attached:db_file2 
      (Problem_diff.select ~file ~kind_diff ~show_rtime_reg ~page))
    >|? Views.render_rounds_diff request ~page ~total 
      ~prover_1:(Round.prover round1) ~prover_2:(Round.prover round2)
  in
  view >>= Helper.view_or_error_to_response request 

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
