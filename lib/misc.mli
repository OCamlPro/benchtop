val pp_list : 'a Fmt.t -> 'a list Fmt.t 
val pp_to_string : 'a Fmt.t -> 'a -> string
val pp_error_code : Unix.process_status Fmt.t 
val sprintf_list : 'a Fmt.t -> 'a list -> string

val from_base64url : string -> (string, [> Error.misc]) result

val now : unit -> Unix.tm

val look_up_get_opt_param : 
  Dream.request -> 
  string -> 
  string option 

val look_up_get_params : 
  Dream.request -> 
  string -> 
  string list 

val look_up_post_param : 
  Dream.request -> 
  string -> 
  (string, [> Error.param]) Lwt_result.t

val look_up_param :
  Dream.request ->
  string ->
  (string, [> Error.param]) Lwt_result.t
