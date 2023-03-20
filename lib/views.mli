val render_rounds_list :
  Dream.request ->
  is_running:bool ->
  (Round.t, Error.t) result list ->
  Models.Prover.t list ->
  string

val render_round_detail :
  Dream.request ->
  page:int ->
  total:int ->
  prover:Models.Prover.t ->
  Models.Round_summary.t ->
  Models.Problem.t list ->
  string

val render_problem_trace : Dream.request -> Models.Problem.t -> string

val render_rounds_diff :
  Dream.request ->
  page:int ->
  total:int ->
  prover_1:Models.Prover.t ->
  prover_2:Models.Prover.t ->
  Models.Problem_diff.t list ->
  string
