module Process : sig 
  type t
  
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
          Dream.error (fun log -> log "Cannot found the database");
          Dream.log "%s" (File.read_all stdout);
          Lwt_result.fail (`Db_not_found rc) 

  let stop {handler; _} = handler#status
  
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
  | Done of {db_file: string; info: Models.Round_summary.t}

type t = {
  config: string;
  date: Unix.tm;
  status: status
}

let make ~cmd ~config = 
  let status = Pending (lazy (Process.run ~cmd ~config)) in
  {config; date = now (); status} 

let retrieve_info db_file =
  let request = 
    Sql.exec ~db_file Sql.round_summary
    |> Sql.debug
  in
  let%lwt date, status = match%lwt request with 
  | Ok (info : Models.Round_summary.t) -> 
      Lwt.return (info.running_at, Done {db_file; info})
  | Error err -> Lwt.return (now (), Failed (`Cannot_retrieve_info err))
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

let problem round name = 
  match round.status with
  | Done {db_file; _} ->
      Sql.exec ~db_file (Sql.select_problem name) 
    |> Sql.debug
  | Pending _ | Running _ | Failed _ ->
      Lwt_result.fail "Not available"

let problems round =
  match round.status with
  | Done {db_file; _} -> 
      Sql.exec ~db_file Sql.select_problems 
      |> Sql.debug
  | Pending _ | Running _ | Failed _ -> 
      Lwt_result.fail "Not available"

