type pending 
type running

type status = 
  | Pending of pending 
  | Running of running 
  | Failed of Error.t
  | Done of {db_file: string; info: Models.Round_summary.t }
  
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

val problem: t -> string -> (Models.Problem.t, string) Lwt_result.t 
val problems : t -> (Models.Problem.t list, string) Lwt_result.t
