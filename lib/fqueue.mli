type 'a t = 'a list * 'a list * 'a list

val empty : 'a t
val push : 'a -> 'a t -> 'a t
val take : 'a t -> 'a option
val pop_map : ('a -> ('a, 'b) Lwt_result.t) -> 'a t -> ('a t, 'b) Lwt_result.t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val pp : 'a Fmt.t -> 'a t Fmt.t
