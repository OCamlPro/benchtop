type 'a t

val empty : 'a t
val push : 'a -> 'a t -> 'a t
val pop : 'a t -> ('a * 'a t) option
val take : 'a t -> 'a option
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val pp : 'a Fmt.t -> 'a t Fmt.t
