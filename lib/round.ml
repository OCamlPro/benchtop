open Syntax

module Process : sig
  type t

  val run : cmd:Lwt_process.command -> t
  val stop : t -> Unix.process_status Lwt.t
  val is_done : t -> bool
  val pp_output : t Fmt.t
end = struct
  type t = {
    handler: Lwt_process.process_none;
    stdout: string;
    stderr: string;
  }

  let run ~cmd =
    let name = Format.sprintf "%s_" (fst cmd) in
    let (stdout, stdout_ch) = Filename.open_temp_file name ".stdout" in
    let stdout_fd = Unix.descr_of_out_channel stdout_ch in
    let (stderr, stderr_ch) = Filename.open_temp_file name ".stderr" in
    let stderr_fd = Unix.descr_of_out_channel stderr_ch in
    let handler = Lwt_process.open_process_none ~stdout:(`FD_move stdout_fd)
      ~stderr:(`FD_move stderr_fd) cmd in
    {handler; stdout; stderr}
  
  let stop {handler; _} =
    handler#terminate;
    handler#status

  let is_done {handler; _} =
    match handler#state with
      | Running -> false
      | Exited _ -> true

  let readall_and_close filename = 
    let ch = open_in filename in
    let content = File.read_all ch in
    In_channel.close ch;
    content

  let pp_output fmt ({stdout; stderr; _} as round) =
    if is_done round then
      Format.fprintf fmt "\
        Standard output: @,\
        %s@,\
        Error output: @,\
        %s@,\
      " 
      (readall_and_close stdout)
      (readall_and_close stderr)
    else 
      Format.fprintf fmt "Output is not ready"
end

type t =
  | Pending of {
    pending_since: Unix.tm;
    cmd: Lwt_process.command;
    provers: Models.Prover.t list
  }
  | Running of {
    running_since: Unix.tm;
    watcher: Lwt_inotify.t;
    proc: Process.t;
    provers: Models.Prover.t list
  }
  | Done of {
    done_since: Unix.tm;
    db_file: string;
    summary: Models.Round_summary.t;
    provers: Models.Prover.t list
  }

let make ~cmd ~provers = Pending {pending_since = Misc.now(); cmd; provers}

let retrieve_info db_file =
  let+? summary =
    Models.retrieve ~db_file
      (Models.Round_summary.retrieve ())
  and+? provers =
    Models.retrieve ~db_file
      (Models.Prover.select ~name:None ~version:None)
  in
  Done {
    done_since = summary.running_at; 
    db_file; 
    summary; 
    provers
  }

let resurect db_file = retrieve_info db_file

let run = function
  | Pending {cmd; provers; _} -> begin
      let name = fst cmd in
      Dream.info (fun log -> log "Ready to run %s" name);
      let proc = Process.run ~cmd in
      Dream.info (fun log -> log "Running %s" name);
      let* watcher = Lwt_inotify.create () in
      let+ _ =
        Lwt_inotify.add_watch watcher Options.benchpress_share_dir
        [Inotify.S_Create]
      in
      Ok (Running {
        running_since = Misc.now ();
        watcher;
        proc;
        provers
      })
    end
  | Running _ | Done _ -> Lwt_result.fail `Is_running

let find_db_file =
  let is_db file =
    String.equal (Filename.extension file) ".sqlite"
  in
  fun inotify ->
    Lwt_inotify.try_read inotify 
    >>= function
      | Some (_, [Inotify.Create], _, Some file) when is_db file -> 
          Lwt_result.return file
      | _ -> Lwt_result.fail `Db_not_found 

let update round =
  match round with
  | Running {proc; watcher; _} -> 
      if Process.is_done proc then
        find_db_file watcher >>= function
        | Ok db_file -> 
            Dream.debug (fun log -> log "Found the database %s" db_file);
            retrieve_info db_file
        | Error err ->
            Dream.debug (fun log -> log "%a" Process.pp_output proc);
            Lwt_result.fail err

      else Lwt_result.return round
  | Pending _ | Done _ -> Lwt_result.return round

let stop = function
  | Running {proc; _} when not @@ Process.is_done proc ->
      let+ rc = Process.stop proc in
      Error (`Stopped rc)
  | (Pending _ | Running _ | Done _) as round -> Lwt_result.return round

let is_done = function
  | Pending _ -> false
  | Running {proc; _} -> Process.is_done proc
  | Done _ -> true

let db_file = function
  | Done {db_file; _} -> Lwt_result.return db_file
  | Pending _ | Running _ -> Lwt_result.fail `Not_done

let provers = function
  | Pending {provers; _} | Running {provers; _} | Done {provers; _} -> provers

let summary = function
  | Done {summary; _} -> Lwt_result.return summary
  | Pending _ | Running _ -> Lwt_result.fail `Not_done

let compare =
  let compare_time t1 t2 = 
    let t1 = Unix.mktime t1 |> fst in
    let t2 = Unix.mktime t2 |> fst in
    Float.compare t1 t2
  in 
  fun round1 round2 -> match round1, round2 with
  | Pending _, Running _ 
  | Running _, Done _ 
  | Pending _, Done _ -> -1
  | Running _, Pending _ 
  | Done _, Running _ 
  | Done _, Pending _ -> 1
  | Pending {pending_since=t1; _}, Pending {pending_since=t2; _} ->
      compare_time t1 t2
  | Running {running_since=t1; _}, Running {running_since=t2; _} ->
      compare_time t1 t2
  | Done {done_since=t1; _}, Done {done_since=t2; _} ->
      compare_time t1 t2

let problem ~name = function
  | Done {db_file; _} ->
      Models.retrieve ~db_file
      (Models.Problem.select_one ~name)
  | Pending _ | Running _ ->
      Lwt_result.fail `Not_done

let problems ?(only_diff=false) ?name ~res ~expected_res ~errcode ~page =
  function
    | Done {db_file; _} ->
        Models.retrieve ~db_file
        (Models.Problem.select ?name ~res ~expected_res
          ~errcode ~only_diff ~page)
    | Pending _ | Running _ -> 
        Lwt_result.fail `Not_done

let count ?(only_diff=false) ?name ~res ~expected_res ~errcode =
  function
    | Done {db_file; _} ->
        Models.retrieve ~db_file
        (Models.Problem.count ?name ~res ~expected_res
          ~errcode ~only_diff)
    | Pending _ | Running _ -> 
        Lwt_result.fail `Not_done


