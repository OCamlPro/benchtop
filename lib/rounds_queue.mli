type t

val make : dir:string -> t Lwt.t
val update : t -> t Lwt.t
val push : Round.t -> t -> t
val to_list : t -> (Round.t, Error.t) result list
val find_by_uuid : string -> t -> (Round.t, [> Error.round]) Lwt_result.t
val is_running: t -> bool
