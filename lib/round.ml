module Process : sig
  type t = private {
    inotify: Lwt_inotify.t;
    handler: Lwt_process.process_none;
    stdout: in_channel;
    stderr: in_channel;
    db_file: string;
  }

  val run : cmd:Lwt_process.command -> config:string ->
    (t, Error.t) Lwt_result.t
  val stop : t -> Unix.process_status Lwt.t
  val is_done : t -> bool
  val db_file : t -> string
end = struct
  type t = {
    inotify: Lwt_inotify.t;
    handler: Lwt_process.process_none;
    stdout: in_channel;
    stderr: in_channel;
    db_file: string;
  }

  let rec wait_db_file =
    let is_db file =
      String.equal (Filename.extension file) ".sqlite"
    in
    fun inotify ->
    match%lwt Lwt_inotify.read inotify with
      | (_, [Inotify.Create], _, Some file) when is_db file -> 
          Lwt_result.return file
      | _ -> wait_db_file inotify

  let wait_terminate handler = 
    let%lwt rc = handler#status in
    Lwt_result.fail rc

  let create_temp_file suffix =
    let file = Filename.temp_file "benchpress" suffix in
    let fd = Unix.openfile file [O_RDWR] 0o640 in
    (fd, Unix.in_channel_of_descr fd)

  let run ~cmd ~config =
    ignore config;
    let%lwt inotify = Lwt_inotify.create () in
    let%lwt _ = 
      Lwt_inotify.add_watch inotify Options.benchpress_share_dir 
        [Inotify.S_Create] 
    in
    let stdout_fd, stdout = create_temp_file "stdout" in 
    let stderr_fd, stderr = create_temp_file "stderr" in 
    Dream.info (fun log -> log "Ready to run benchpress");
    let handler = Lwt_process.open_process_none ~stdout:(`FD_copy stdout_fd) 
      ~stderr:(`FD_copy stderr_fd) cmd in
    Dream.info (fun log -> log "Running benchpress");
    match%lwt Lwt.choose [wait_db_file inotify; 
        wait_terminate handler] with
      | Ok db_file -> 
          Dream.info (fun log -> log "Found the database %s" db_file);
          Lwt_result.return {inotify; stdout; stderr; handler; db_file}
      | Error rc ->
          Dream.error (fun log -> log "\
            Cannot found the database@,\
            Error code: %a@,\
            Standard output: @,\
            %s@,\
            Error output: @,\
            %s@,\
            " 
            Misc.pp_error_code rc 
            (File.read_all stdout) 
            (File.read_all stderr));
          Lwt_result.fail (`Db_not_found rc) 

  let stop {handler; stdout; stderr; db_file; _} = 
    handler#terminate;
    let%lwt rc = handler#status in
    In_channel.close stdout;
    In_channel.close stderr;
    Unix.unlink db_file;
    Lwt.return rc
  
  let is_done {handler; _} =
    match handler#state with
    | Running -> false
    | Exited _ -> true 

  let db_file {db_file; _} = db_file
end

let now () = Unix.time () |> Unix.localtime

type pending = (Process.t, Error.t) Lwt_result.t Lazy.t 
type running = Process.t 

type status = 
  | Pending of pending
  | Running of Process.t
  | Failed of Error.t
  | Done of {
    db_file: string;
    summary: Models.Round_summary.t;
    provers: Models.Prover.t list
  }

type t = {
  config: string;
  date: Unix.tm;
  status: status
}

let make ~cmd ~config = 
  let status = Pending (lazy (Process.run ~cmd ~config)) in
  {config; date = now (); status} 

let retrieve_info db_file =
  let request_summary = 
    Models.retrieve ~db_file 
      (Models.Round_summary.retrieve ()) 
  in
  let request_provers = 
    Models.retrieve ~db_file 
      (Models.Prover.select ~name:None ~version:None)
  in
  let%lwt date, status = 
    match%lwt Lwt.both request_summary request_provers with 
  | Ok (summary : Models.Round_summary.t), 
    Ok (provers : Models.Prover.t list) -> 
      Lwt.return (summary.running_at, Done {db_file; summary; provers})
  | Error err, _ -> Lwt.return (now (), Failed err)
  | _, Error err -> Lwt.return (now (), Failed err)
  in
  Lwt.return {config = "default"; date; status}

let resurect ~db_file = retrieve_info db_file

let run round = 
  match round.status with
  | Pending susp -> begin
      let%lwt status = match%lwt Lazy.force susp with
      | Ok proc -> Lwt.return (Running proc)
      | Error err -> Lwt.return (Failed err)
      in
      Lwt.return {round with date = now(); status}
    end
  | Running _ | Failed _ | Done _ -> Lwt.return round 

let update round =
  match round.status with
  | Running proc ->
      if Process.is_done proc then
        Process.db_file proc |> retrieve_info 
      else Lwt.return round
  | Pending _ | Failed _ | Done _ -> Lwt.return round

let stop round =
  match round.status with
  | Running proc when not @@ Process.is_done proc -> 
      let%lwt rc = Process.stop proc in
      Lwt.return {round with date = now (); status = Failed (`Stopped rc)}
  | Pending _ | Running _ | Failed _ | Done _ -> Lwt.return round 

let is_done round =
  match round.status with
  | Pending _ -> false
  | Running proc -> Process.is_done proc
  | Failed _ | Done _ -> true

let problem ~name round = 
  match round.status with
  | Done {db_file; _} ->
      Models.retrieve ~db_file 
      (Models.Problem.select_one ~name) 
  | Pending _ | Running _ | Failed _ ->
      Lwt_result.fail `Not_done

let problems ?(only_diff=false) ?name ?res ?expected_res ?errcode round =
  match round.status with
  | Done {db_file; _} -> 
      Models.retrieve ~db_file
      (Models.Problem.select ~name ~res ~expected_res ~errcode ~only_diff)
  | Pending _ | Running _ | Failed _ -> 
      Lwt_result.fail `Not_done

