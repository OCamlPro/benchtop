open Syntax

module Process : sig
  type t = private {
    inotify: Lwt_inotify.t;
    handler: Lwt_process.process_none;
    stdout: in_channel;
    stderr: in_channel;
    db_file: string;
  }

  val run :
    cmd:Lwt_process.command ->
    config:string ->
     (t, [> Error.process]) Lwt_result.t

  val stop : t -> Unix.process_status Lwt.t
  val is_done : t -> bool
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
      Lwt_inotify.read inotify 
      >>= function
        | (_, [Inotify.Create], _, Some file) when is_db file -> 
            Lwt_result.return file
        | _ -> wait_db_file inotify

  let wait_terminate handler = 
    handler#status >|= fun rc -> Error rc

  let run ~cmd ~config =
    ignore config;
    let* inotify = Lwt_inotify.create () in
    let* _ =
      Lwt_inotify.add_watch inotify Options.benchpress_share_dir
        [Inotify.S_Create]
    in
    let (_, stdout) = File.to_temp_file "benchpress" "stdout" in
    let stdout_fd = Unix.descr_of_in_channel stdout in
    let (_, stderr) = File.to_temp_file "benchpress" "stderr" in
    let stderr_fd = Unix.descr_of_in_channel stderr in
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
            Cannot find the database@,\
            Error code: %a@,\
            Standard output: @,\
            %s@,\
            Error output: @,\
            %s@,\
            " 
            Misc.pp_error_code rc
            (File.read_all stdout)
            (File.read_all stderr));
          Lwt_result.fail `Db_not_found

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

type status =
  | Pending
  | Running of Process.t
  | Done of {
    db_file: string;
    summary: Models.Round_summary.t;
    provers: Models.Prover.t list
  }

type t = {
  cmd: Lwt_process.command;
  config: string;
  date: Unix.tm;
  status: status
}

let make ~cmd ~config =
  {cmd; config; date = now (); status = Pending}

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
  {cmd = ("",[||]); config = "default"; date; status}

let resurect ~db_file = retrieve_info ~db_file

let run ({cmd; config; status; _} as round) =
  match status with
  | Pending -> begin
      let+? proc = Process.run ~cmd ~config in
      {round with date = now (); status = Running proc}
    end
  | Running _ | Done _ -> Lwt_result.fail `Is_running

let update ({status; _} as round) =
  match status with
  | Running ({db_file; _} as trace) ->
    if Process.is_done trace then
      retrieve_info ~db_file
    else Lwt_result.return round
  | Pending | Done _ -> Lwt_result.return round

let stop ({status; _} as round) =
  match status with
  | Running trace when not @@ Process.is_done trace ->
      let+ rc = Process.stop trace in
      Error (`Stopped rc)
  | Pending | Running _ | Done _ -> Lwt_result.return round

let db_file {status; _} =
  match status with
  | Pending | Running _ -> Lwt_result.fail `Db_not_found
  | Done {db_file; _} -> Lwt_result.return db_file

let is_done {status; _} =
  match status with
  | Pending -> false
  | Running proc -> Process.is_done proc
  | Done _ -> true

let problem ~name {status; _} =
  match status with
  | Done {db_file; _} ->
      Models.retrieve ~db_file
      (Models.Problem.select_one ~name)
  | Pending | Running _ ->
      Lwt_result.fail `Not_done

let problems ?(only_diff=false) ?name ?res ?expected_res ?errcode {status; _} =
  match status with
  | Done {db_file; _} -> 
      Models.retrieve ~db_file
      (Models.Problem.select ~name ~res ~expected_res ~errcode ~only_diff)
  | Pending | Running _ -> 
      Lwt_result.fail `Not_done

