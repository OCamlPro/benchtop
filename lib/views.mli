type view = Dream.request -> string 

val render_404_not_found : view
val render_rounds_list : is_running:bool -> Round.t list -> view
val render_round_detail : Models.Problem.t list -> view
val render_problem_trace : Models.Problem.t -> view
