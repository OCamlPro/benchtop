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

type param = [ form | `Key_not_found of string ]
type misc = [ `Cannot_convert_to_base64 | `Unknown_error of string ]

type t = [ sql | round | param | misc ]

let pp_error_code fmt =
  let open Unix in
  function
  | WEXITED rc -> Format.fprintf fmt "WEXITED %i" rc
  | WSIGNALED rc -> Format.fprintf fmt "WSIGNALED %i" rc
  | WSTOPPED rc -> Format.fprintf fmt "WSTOPPED %i" rc

let pp_process fmt = function
  | `Is_running -> Format.fprintf fmt "The processus is still running"
  | `Stopped rc ->
      Format.fprintf fmt "The processus has been stopped with the error code %a"
        pp_error_code rc
  | `Db_not_found -> Format.fprintf fmt "The database has not been found"

let pp_round fmt = function
  | #Caqti_error.t as err -> Caqti_error.pp fmt err
  | #process as err -> pp_process fmt err
  | `Round_not_found -> Format.fprintf fmt "Cannot find the round of uuid ?"
  | `Cannot_retrieve_info db_file ->
      Format.fprintf fmt "Cannot retrieve data from the database %s" db_file
  | `Not_done -> Format.fprintf fmt "The round is still running"

let pp_form _fmt = function _ -> failwith "not implemented yet"

let pp_param fmt = function
  | #form as err -> pp_form fmt err
  | `Key_not_found str -> Format.fprintf fmt "Cannot found the key %s" str

let pp_misc fmt = function
  | `Cannot_convert_to_base64 ->
      Format.fprintf fmt "Cannot convert the string to base64"
  | `Unknown_error err -> Format.fprintf fmt "Unknown error: %s" err

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
