type pending
type running

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
end

type status = private
  | Pending of pending
  | Running of Process.t
  | Failed of Error.t
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
val resurect : db_file:string -> t Lwt.t
val run : t -> t Lwt.t
val update : t -> t Lwt.t
val stop : t -> t Lwt.t
val is_done : t -> bool

val problem:
  name:string ->
  t ->
  (Models.Problem.t, Error.t) Lwt_result.t

val problems :
  ?name: string ->
  ?res: Models.Fields.Res.t ->
  ?expected_res: Models.Fields.Res.t ->
  ?errcode: Models.Fields.Errcode.t ->
  t -> (Models.Problem.t list, Error.t) Lwt_result.t
