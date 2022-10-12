type 'a printer = Format.formatter -> 'a -> unit

val pp_list : 'a printer -> 'a list printer
val pp_error_code : Unix.process_status printer
val sprintf_list : 'a printer -> 'a list -> string

