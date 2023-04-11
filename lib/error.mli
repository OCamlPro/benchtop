type sql = Caqti_error.t
type process = [ `Is_running | `Stopped of Unix.process_status | `Db_not_found ]

type round =
  [ sql
  | process
  | `Round_not_found
  | `Cannot_retrieve_info of string
  | `Not_done
  | `Not_running ]

type form =
  [ `Expired of (string * string) list * float
  | `Wrong_session of (string * string) list
  | `Invalid_token of (string * string) list
  | `Missing_token of (string * string) list
  | `Many_tokens of (string * string) list
  | `Wrong_content_type ]

type param = [ form | `Key_not_found of string | `Key_wrong_type of string ]
type misc =
  [ `Cannot_convert_to_base64
  | `Unknown_error of string
  | `Cannot_read of string ]

(* TODO: catch exception and transform them to an error of type t. *)
type t = [ sql | process | round | param | form | misc ]

val pp : [< t ] Fmt.t
val show : [< t ] -> string

val get_session : Dream.request -> [< t ] option
(** [get_session request] return the error produced by the previous request
    if any. *)

val set_session : Dream.request -> [< t ] -> unit
(** [set_session request err] set the error [err] to be retrieved by the next
    request. *)
