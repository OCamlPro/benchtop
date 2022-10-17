val render_error : msg:string -> string
val render_rounds_list : 
  Dream.request -> 
  is_running:bool -> 
  Round.t list -> 
  (string, Error.t) Lwt_result.t

val render_round_detail : 
  Dream.request -> 
  Models.Problem.t list -> 
  (string, Error.t) Lwt_result.t

val render_problem_trace : 
  Dream.request -> 
  Models.Problem.t -> 
  (string, Error.t) Lwt_result.t

val render_rounds_diff : 
  Dream.request -> 
  Models.Problem_diff.t list -> 
  (string, Error.t) Lwt_result.t
