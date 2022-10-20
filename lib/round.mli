type pending
type running

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
end

type status = private
  | Pending of Process.t
  | Running of Process.trace
  | Done of {
    db_file: string;
    summary: Models.Round_summary.t;
    provers: Models.Prover.t list
  }

type t = private {
  config: string;
  date: Unix.tm;
  status: status
}

val make : cmd:Lwt_process.command -> config:string -> t
val resurect : db_file:string -> (t, Error.t) Lwt_result.t
val run : t -> (t, Error.t) Lwt_result.t
val update : t -> (t, Error.t) Lwt_result.t
val stop : t -> (t, Error.t) Lwt_result.t
val db_file : t -> (string, Error.t) Result.t
val is_done : t -> bool

val problem:
  name:string ->
  t ->
  (Models.Problem.t, Error.t) Lwt_result.t

val problems :
  ?only_diff: bool ->
  ?name: string ->
  ?res: Models.Fields.Res.t ->
  ?expected_res: Models.Fields.Res.t ->
  ?errcode: Models.Fields.Errcode.t ->
  t -> (Models.Problem.t list, Error.t) Lwt_result.t
