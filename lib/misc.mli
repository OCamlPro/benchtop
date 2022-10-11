type 'a printer = Format.formatter -> 'a -> unit

val sprintf_list : 'a printer -> 'a list -> string
val pp_list : 'a printer -> 'a list printer

