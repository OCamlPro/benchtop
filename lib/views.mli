val render_error : msg:string -> string
val render_rounds_list : 
  Dream.request -> 
  is_running:bool -> 
  (Round.t, Error.t) result list -> 
  string

val render_round_detail : 
  Dream.request -> 
  Models.Problem.t list -> 
  string 

val render_problem_trace : 
  Dream.request -> 
  Models.Problem.t -> 
  string

val render_rounds_diff : 
  Dream.request -> 
  Models.Problem_diff.t list -> 
  string
