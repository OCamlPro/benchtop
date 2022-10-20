open Syntax

module Process : sig
  type t

  type trace = private {
    inotify: Lwt_inotify.t;
    handler: Lwt_process.process_none;
    stdout: in_channel;
    stderr: in_channel;
    db_file: string;
  }

  val make :
    cmd:Lwt_process.command ->
    config:string ->
    t

  val run : t -> (trace, Error.t) Lwt_result.t
  val stop : trace -> Unix.process_status Lwt.t
  val is_done : trace -> bool
end = struct
  type trace = {
    inotify: Lwt_inotify.t;
    handler: Lwt_process.process_none;
    stdout: in_channel;
    stderr: in_channel;
    db_file: string;
  }

  type t = (trace, Error.t) Lwt_result.t

  let rec wait_db_file =
    let is_db file =
      String.equal (Filename.extension file) ".sqlite"
    in
    fun inotify ->
      Lwt_inotify.read inotify 
      >>= function
        | (_, [Inotify.Create], _, Some file) when is_db file -> 
            Lwt_result.return file
        | _ -> wait_db_file inotify

  let wait_terminate handler = 
    handler#status >|= fun rc -> Error rc

  let create_temp_file suffix =
    let file = Filename.temp_file "benchpress" suffix in
    let fd = Unix.openfile file [O_RDWR] 0o640 in
    (fd, Unix.in_channel_of_descr fd)

  let make ~cmd ~config =
    ignore config;
    let* inotify = Lwt_inotify.create () in
    let* _ =
      Lwt_inotify.add_watch inotify Options.benchpress_share_dir
        [Inotify.S_Create]
    in
    let stdout_fd, stdout = create_temp_file "stdout" in
    let stderr_fd, stderr = create_temp_file "stderr" in
    Dream.info (fun log -> log "Ready to run benchpress");
    let handler = Lwt_process.open_process_none ~stdout:(`FD_copy stdout_fd)
      ~stderr:(`FD_copy stderr_fd) cmd in
    Dream.info (fun log -> log "Running benchpress");
    Lwt.choose [wait_db_file inotify; wait_terminate handler]
    >>= function
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

  let run proc = proc

  let stop {handler; stdout; stderr; db_file; _} =
    handler#terminate;
    In_channel.close stdout;
    In_channel.close stderr;
    Unix.unlink db_file;
    handler#status
 
  let is_done {handler; _} =
    match handler#state with
      | Running -> false
      | Exited _ -> true
end

let now () = Unix.time () |> Unix.localtime

type pending = (Process.t, Error.t) Lwt_result.t
type running = Process.t

type status =
  | Pending of Process.t
  | Running of Process.trace
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
  let status = Pending (Process.make ~cmd ~config) in
  {config; date = now (); status}

let retrieve_info ~db_file =
  let+? summary =
    Models.retrieve ~db_file
      (Models.Round_summary.retrieve ())
  and+? provers =
    Models.retrieve ~db_file
      (Models.Prover.select ~name:None ~version:None)
  in
  let date, status =
    (summary.running_at, Done {db_file; summary; provers})
  in
  {config = "default"; date; status}

let resurect ~db_file = retrieve_info ~db_file

let run round =
  match round.status with
  | Pending susp -> begin
      let+? trace = Process.run susp in
      {round with date = now (); status = Running trace}
    end
  | Running _ | Done _ -> Lwt_result.fail `Is_running

let update round =
  match round.status with
  | Running ({db_file; _} as trace) ->
    if Process.is_done trace then
      retrieve_info ~db_file
    else Lwt_result.return round
  | Pending _ | Done _ -> Lwt_result.return round

let stop round =
  match round.status with
  | Running trace when not @@ Process.is_done trace ->
      let+ rc = Process.stop trace in
      Error (`Stopped rc)
  | Pending _ | Running _ | Done _ -> Lwt_result.return round

let db_file round =
  match round.status with
  | Pending _ | Running _ -> Error `Not_found
  | Done {db_file; _} -> Ok db_file

let is_done round =
  match round.status with
  | Pending _ -> false
  | Running proc -> Process.is_done proc
  | Done _ -> true

let problem ~name round =
  match round.status with
  | Done {db_file; _} ->
      Models.retrieve ~db_file
      (Models.Problem.select_one ~name)
  | Pending _ | Running _ ->
      Lwt_result.fail `Not_done

let problems ?(only_diff=false) ?name ?res ?expected_res ?errcode round =
  match round.status with
  | Done {db_file; _} -> 
      Models.retrieve ~db_file
      (Models.Problem.select ~name ~res ~expected_res ~errcode ~only_diff)
  | Pending _ | Running _ -> 
      Lwt_result.fail `Not_done

