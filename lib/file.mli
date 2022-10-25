val cat : Format.formatter -> string -> unit
val readdir : ?ext_filter:(string -> bool) -> string -> string list
val read_all : in_channel -> string
val to_temp_file : string -> string -> string * in_channel
