val pp_list : 'a Fmt.t -> 'a list Fmt.t 
val pp_error_code : Unix.process_status Fmt.t 
val sprintf_list : 'a Fmt.t -> 'a list -> string

val from_base64url : string -> (string, [> Error.misc]) result

val now : unit -> Unix.tm
