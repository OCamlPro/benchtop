type t = { queue : Rounds_queue.t }

val init : unit -> unit Lwt.t
val get : unit -> t
val set : t -> unit
val update : unit -> unit Lwt.t
