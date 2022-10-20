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
end

type status = private
  | Pending
  | Running of Process.t
  | Done of {
    db_file: string;
    summary: Models.Round_summary.t;
    provers: Models.Prover.t list
  }

type t = private {
  cmd: Lwt_process.command;
  config: string;
  date: Unix.tm;
  status: status
}

val make : cmd:Lwt_process.command -> config:string -> t
val resurect : db_file:string -> (t, [> Error.round]) Lwt_result.t
val run : t -> (t, [> Error.round]) Lwt_result.t
val update : t -> (t, [> Error.round]) Lwt_result.t
val stop : t -> (t, [> Error.round]) Lwt_result.t
val db_file : t -> (string, [> Error.round]) Lwt_result.t
val is_done : t -> bool

val problem:
  name:string ->
  t ->
    (Models.Problem.t, [> Error.round]) Lwt_result.t

val problems :
  ?only_diff: bool ->
  ?name: string ->
  ?res: Models.Fields.Res.t ->
  ?expected_res: Models.Fields.Res.t ->
  ?errcode: Models.Fields.Errcode.t ->
  t -> (Models.Problem.t list, [> Error.round]) Lwt_result.t
