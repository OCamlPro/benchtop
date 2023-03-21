val cat : Format.formatter -> string -> unit

val read_dir : ?ext_filter:(string -> bool) -> string -> string list

val read_lines : ?count:int -> string -> (string list, _) Lwt_result.t
(** [read_lines ?count file] read the content of the file [file].
    If the argument [count] is given, the function stops as soon as we have read
    at most [count] lines. *)

val extract_zip_file : string -> (string, Error.misc) result
(** [extract_zip_file file] extract and read all the unique entry of the
    zip archive [zip]. *)

val to_temp_file : string -> string -> string * in_channel
