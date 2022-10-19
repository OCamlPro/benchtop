type t = {
  mutable queue: Rounds_queue.t;
}

val init : unit -> t
val retrieve : Dream.request -> t
val middleware : t -> Dream.middleware

