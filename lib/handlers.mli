type ctx = { 
  mutable queue: Rounds_queue.t
}

val handle_rounds_list : ctx Lwt.t -> Dream.handler
val handle_round_detail : ctx Lwt.t -> Dream.handler
val handle_rounds_diff : ctx Lwt.t -> Dream.handler
val handle_round_action_dispatcher : ctx Lwt.t -> Dream.handler
val handle_problem_trace : ctx Lwt.t -> Dream.handler
val handle_schedule_round : ctx Lwt.t -> Dream.handler
val handle_stop_round : ctx Lwt.t -> Dream.handler
