module Process : sig
  type t

  val run : cmd:Lwt_process.command -> t
  val stop : t -> Unix.process_status Lwt.t
  val is_done : t -> bool
  val pp_output : t Fmt.t
end

type t = private
  | Pending of {
    pending_since: Unix.tm;
    cmd: Lwt_process.command;
    prover: Models.Prover.t
  }
  | Running of {
    running_since: Unix.tm;
    watcher: Lwt_inotify.t;
    proc: Process.t;
    prover: Models.Prover.t
  }
  | Done of {
    done_since: Unix.tm;
    db_file: string;
    summary: Models.Round_summary.t;
    prover: Models.Prover.t
  }

val make : cmd:Lwt_process.command -> prover:Models.Prover.t -> t
val resurect : string -> (t, [> Error.round]) Lwt_result.t
val run : t -> (t, [> Error.round]) Lwt_result.t
val update : t -> (t, [> Error.round]) Lwt_result.t
val stop : t -> (t, [> Error.round]) Lwt_result.t
val db_file : t -> (string, [> Error.round]) Lwt_result.t
val summary : t -> (Models.Round_summary.t, [> Error.round]) Lwt_result.t
val is_done : t -> bool
val prover : t -> Models.Prover.t
val compare : t -> t -> int

val problem:
  name:string ->
  t ->
  (Models.Problem.t, [> Error.round]) Lwt_result.t

val problems :
  ?only_diff: bool ->
  ?file: string ->
  res: Models.Res.t list ->
  file_expect: Models.Res.t list ->
  errcode: Models.Errcode.t list ->
  page: int ->
  t -> (Models.Problem.t list, [> Error.round]) Lwt_result.t

val count :
  ?only_diff: bool ->
  ?file: string ->
  res: Models.Res.t list ->
  file_expect: Models.Res.t list ->
  errcode: Models.Errcode.t list ->
  t -> (int, [> Error.round]) Lwt_result.t
