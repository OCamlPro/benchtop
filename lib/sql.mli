type 'a request 
type 'a answer = ('a, Caqti_error.t) Lwt_result.t

val exec : db_file:string -> 'a request -> 'a answer
val compose : ('a -> 'b -> 'c) -> 'a request -> 'b request -> 'c request
val debug : 'a answer -> ('a, string) Lwt_result.t
val attach : db_file:string -> unit request

val round_summary : Models.Round_summary.t request
val provers : Models.Prover.t list request
val set_wal : int request
val compare : Models.Problem_diff.t list request
