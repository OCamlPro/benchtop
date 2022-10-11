type 'a request 
type 'a answer = ('a, Caqti_error.t) Lwt_result.t

val exec : db_file:string -> 'a request -> 'a answer
val (@) : 'a request -> 'b request -> ('a * 'b) request
val debug : 'a answer -> ('a, string) Lwt_result.t

val round_summary : Models.Round_summary.t request
val select_problem : string -> Models.Problem.t request
val select_problems : Models.Problem.t list request
val provers : Models.Prover.t list request
val set_wal : int request
