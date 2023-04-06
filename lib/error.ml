type sql = Caqti_error.t
type process = [ `Is_running | `Stopped of Unix.process_status | `Db_not_found ]

type round =
  [ sql
  | process
  | `Round_not_found
  | `Cannot_retrieve_info of string
  | `Not_done ]

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

type t = [ sql | round | param | misc ]

let pp_error_code fmt =
  let open Unix in
  function
  | WEXITED rc -> Fmt.pf fmt "WEXITED %i" rc
  | WSIGNALED rc -> Fmt.pf fmt "WSIGNALED %i" rc
  | WSTOPPED rc -> Fmt.pf fmt "WSTOPPED %i" rc

let pp_process fmt = function
  | `Is_running -> Fmt.pf fmt "The processus is still running"
  | `Stopped rc ->
      Fmt.pf fmt "The processus has been stopped with the error code %a"
        pp_error_code rc
  | `Db_not_found -> Fmt.pf fmt "The database has not been found"

let pp_round fmt = function
  | #Caqti_error.t as err -> Caqti_error.pp fmt err
  | #process as err -> pp_process fmt err
  | `Round_not_found -> Fmt.pf fmt "Cannot find the round of uuid ?"
  | `Cannot_retrieve_info db_file ->
      Fmt.pf fmt "Cannot retrieve data from the database %s" db_file
  | `Not_done -> Fmt.pf fmt "The round is still running"

(* TODO: implement this function. *)
let pp_form _fmt = function _ -> failwith "not implemented yet"

let pp_param fmt = function
  | #form as err -> pp_form fmt err
  | `Key_not_found str -> Fmt.pf fmt "Cannot find the key %s" str
  | `Key_wrong_type str ->
      Fmt.pf fmt "Wrong type value for the key %s" str

let pp_misc fmt = function
  | `Cannot_convert_to_base64 ->
      Fmt.pf fmt "Cannot convert the string to base64"
  | `Unknown_error err -> Fmt.pf fmt "Unknown error: %s" err
  | `Cannot_read file -> Fmt.pf fmt "Cannot read the file %s" file

let pp fmt = function
  | #Caqti_error.t as err -> Caqti_error.pp fmt err
  | #round as err -> pp_round fmt err
  | #param as err -> pp_param fmt err
  | #misc as err -> pp_misc fmt err

let show err =
  pp Format.str_formatter err;
  Format.flush_str_formatter ()

let get_session request =
  Dream.flash_messages request |> List.assoc_opt "error" |> function
  | Some str -> Marshal.from_string str 0
  | None -> None

let set_session request err =
  let err =
    match err with
    | #sql as err ->
        (* Some Caqti errors are functional and cannot be
             marshaled.
             TODO: preserve Caqti_error.t s that can be marshaled
             and translate the others. *)
        `Unknown_error (Caqti_error.show err)
    | (#round | #param | #misc) as err -> err
  in
  Marshal.to_string (Some err) [] |> Dream.add_flash_message request "error"
