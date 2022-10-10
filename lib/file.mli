val cat : Format.formatter -> string -> unit
val readdir : ?ext_filter:(string -> bool) -> string -> string list
val read_all : in_channel -> string
