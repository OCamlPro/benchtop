val handle_rounds_list : Dream.handler
val handle_round_detail : Dream.handler
val handle_rounds_diff : Dream.handler
val handle_round_action_dispatcher : Dream.handler
val handle_problem_trace : Dream.handler
val handle_schedule_round : j:int -> Dream.handler
(** [handle_schedule_round ~j hdl] schedule a new rounds a new round of tests
    with [j] jobs. *)

val handle_stop_round : Dream.handler
val handle_problems_list : Dream.handler
