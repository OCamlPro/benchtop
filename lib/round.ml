open Syntax

module Process : sig
  type t

  val run : cmd:Lwt_process.command -> t
  val stop : t -> Unix.process_status Lwt.t
  val is_done : t -> bool
  val pp_output : t Fmt.t
end = struct
  type t = {
    handler : Lwt_process.process_none;
    stdout : string;
    stderr : string;
  }

  let run ~cmd =
    let name = Format.sprintf "%s_" (fst cmd) in
    let stdout, stdout_ch = Filename.open_temp_file name ".stdout" in
    let stdout_fd = Unix.descr_of_out_channel stdout_ch in
    let stderr, stderr_ch = Filename.open_temp_file name ".stderr" in
    let stderr_fd = Unix.descr_of_out_channel stderr_ch in
    let handler =
      Lwt_process.open_process_none ~stdout:(`FD_move stdout_fd)
        ~stderr:(`FD_move stderr_fd) cmd
    in
    { handler; stdout; stderr }

  let stop { handler; _ } =
    handler#terminate;
    handler#status

  let is_done { handler; _ } =
    match handler#state with Running -> false | Exited _ -> true

  let pp_list fmt =
    let pp_sep fmt () = Fmt.pf fmt "\n" in
    let pp_string fmt = Fmt.pf fmt "%s" in
    Format.pp_print_list ~pp_sep pp_string fmt

  let pp_output fmt ({ stdout; stderr; _ } as proc) = ()
    (* if is_done proc then
      let* stdout = File.read_until stdout in
      let* stderr = File.read_until stderr in
      Fmt.pf fmt "Standard output: @,%a@,Error output: @,%a@," pp_list stdout pp_list stderr;
      Lwt.return_unit
    else Fmt.pf fmt "Output is not ready"; Lwt.return_unit *)
end

type status =
  | Pending of {
      pending_since : Unix.tm;
      cmd : Lwt_process.command;
    }
  | Running of {
      running_since : Unix.tm;
      watcher : Lwt_inotify.t;
      proc : Process.t;
    }
  | Done of {
      done_since : Unix.tm;
      summary : Models.Round_summary.t;
    }

type t = {
    id : Uuidm.t;
    prover : Models.Prover.t;
    status : status;
  }

let pp_bp_config ~binary fmt () =
  let binary_path = Filename.concat Options.binaries_dir binary in
  let prover = Models.Prover.of_binary_name binary in
  Format.fprintf fmt
    ("\
  (import-prelude false)@\n\
  (prover@ \
    @[<v 2>(name ae-read-status)@ \
    (cmd \"zgrep :status $file\")@ \
    (unknown \":status unknown\")@ \
    (unknown \"\")@ \
    (sat \":status sat\")@ \
    (unsat \":status unsat|:status valid\"))@]@\n\
  (dir@ \
    @[<v 2>(path \"%s\")@ \
    (pattern \".*.ae|.*.smt2|.*.zip\")@ \
    (expect (run ae-read-status)))@]@\n\
  (prover@ \
    @[<v 2>(name %s)@ \
    (version %s)@ \
    (cmd \"%s --timelimit=$timeout $file\")@ \
    (sat \"^sat\")@ \
    (unsat \"Valid|(^unsat)\")@ \
    (unknown \"(I Don't Know)|(^unsat)\")@ \
    (timeout \"^timeout\"))@]@."[@ocamlformat "disable"] )
    Options.tests_dir prover.name prover.version binary_path

let generate_bp_config ~binary =
  let filename, ch = Filename.open_temp_file "benchpress_" ".sexp" in
  let fmt = Format.formatter_of_out_channel ch in
  pp_bp_config ~binary fmt ();
  Dream.debug (fun log -> log "%a" (pp_bp_config ~binary) ());
  close_out ch;
  filename

let pp_cmd fmt (_, args) =
  let pp_sep fmt () = Format.fprintf fmt " " in
  let pp_string fmt = Format.fprintf fmt "%s" in
  Format.pp_print_list ~pp_sep pp_string fmt (Array.to_list args)

let make ~binary =
  let now = Misc.now () in
  let id = Models.Time.show now |> String.to_bytes |> Uuidm.v4 in
  let prover = Models.Prover.of_binary_name binary in
  let config_path = generate_bp_config ~binary in
  let cmd =
        ( "benchpress",
          [|
            "benchpress";
            "run";
            "--no-failure";
            "-o";
            Filename.concat Options.db_dir (Format.asprintf "%a.sqlite" Uuidm.pp id);
            "-j";
            string_of_int Options.number_of_jobs;
            "-c";
            config_path;
            "-t";
            string_of_int Options.prover_timeout;
            "-p";
            "alt-ergo";
            Options.tests_dir;
          |] )
  in
  Dream.debug (fun log -> log "Command: %a" pp_cmd cmd);
  { id; prover; status = Pending { pending_since = now; cmd } }

let retrieve_info db_file =
  let+? summary = Models.retrieve ~db_file (Models.Round_summary.retrieve ())
  and+? prover =
    Models.retrieve ~db_file (Models.Prover.select ~name:None ~version:None)
    >|? List.hd
  in
  (* TODO: generate an error if we fail to retrieve the uuid from the
     filename. *)
  let id = Filename.chop_extension db_file |> Uuidm.of_string |> Option.get in
  { id; prover; status = Done { done_since = summary.running_at; summary } }

let resurect db_file = retrieve_info db_file

let run { id; prover; status; _ } =
  match status with
  | Pending { cmd; _ } ->
      let name = fst cmd in
      let* watcher = Lwt_inotify.create () in
      let+ _ =
        Lwt_inotify.add_watch watcher Options.db_dir [ Inotify.S_Create ]
      in
      Dream.debug (fun log -> log "Ready to run %s" name);
      let proc = Process.run ~cmd in
      Dream.debug (fun log -> log "Running %s" name);
      Ok ({ id; prover; status = Running { running_since = Misc.now (); watcher; proc } })
  | Running _ | Done _ -> Lwt_result.fail `Is_running

let db_file { id; _ } = Format.sprintf "%s.sqlite" (Uuidm.to_string id)

let find_db_file round inotify =
  let is_my_db = String.equal (db_file round) in
  Dream.debug (fun log -> log "DB NAME: %s" (db_file round));
  Lwt_inotify.try_read inotify >>= function
  | Some (_, [ Inotify.Create ], _, Some file) when is_my_db file ->
      Lwt_result.return file
  | _ -> Lwt_result.fail `Db_not_found

let update ({ status; _ } as round) =
  match status with
  | Running { proc; watcher; _ } ->
      if Process.is_done proc then (
        find_db_file round watcher >>= function
        | Ok db_file ->
            Dream.debug (fun log -> log "Found the database %s" db_file);
            retrieve_info db_file
        | Error err ->
            Dream.debug (fun log -> log "%a" Process.pp_output proc);
            Lwt_result.fail err)
      else Lwt_result.return round
  | Pending _ | Done _ -> Lwt_result.return round

let stop ({ status; _ } as round) =
  match status with
  | Running { proc; _ } when not @@ Process.is_done proc ->
      let+ rc = Process.stop proc in
      Error (`Stopped rc)
  | Pending _ | Running _ | Done _ -> Lwt_result.return round

let is_done { status; _ } =
  match status with
  | Pending _ -> false
  | Running { proc; _ } -> Process.is_done proc
  | Done _ -> true

let summary { status; _ } =
  match status with
  | Done { summary; _ } -> Lwt_result.return summary
  | Pending _ | Running _ -> Lwt_result.fail `Not_done

let compare =
  let compare_time t1 t2 =
    let t1 = Unix.mktime t1 |> fst in
    let t2 = Unix.mktime t2 |> fst in
    Float.compare t1 t2
  in
  fun { status = round1; _ } { status = round2; _ } ->
    match (round1, round2) with
    | Pending _, Running _ | Running _, Done _ | Pending _, Done _ -> -1
    | Running _, Pending _ | Done _, Running _ | Done _, Pending _ -> 1
    | Pending { pending_since = t1; _ }, Pending { pending_since = t2; _ } ->
        compare_time t1 t2
    | Running { running_since = t1; _ }, Running { running_since = t2; _ } ->
        compare_time t1 t2
    | Done { done_since = t1; _ }, Done { done_since = t2; _ } ->
        compare_time t1 t2

let problem ~name ({ status; _ } as round) =
  match status with
  | Done _ ->
      Models.retrieve ~db_file:(db_file round) (Models.Problem.select_one ~name)
  | Pending _ | Running _ -> Lwt_result.fail `Not_done

let problems ?(only_diff = false) ?file ~res ~file_expect ~errcode ~page
  ({ status; _ } as round) =
  match status with
  | Done _ ->
      Models.retrieve ~db_file:(db_file round)
        (Models.Problem.select ?file ~res ~file_expect ~errcode ~only_diff ~page)
  | Pending _ | Running _ -> Lwt_result.fail `Not_done

let count ?(only_diff = false) ?file ~res ~file_expect ~errcode
  ({ status; _ } as round) =
  match status with
  | Done _ ->
      Models.retrieve ~db_file:(db_file round)
        (Models.Problem.count ?file ~res ~file_expect ~errcode ~only_diff)
  | Pending _ | Running _ -> Lwt_result.fail `Not_done
