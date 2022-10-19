type sql_error = Caqti_error.t 

type process_error = [
  | `Stopped of Unix.process_status
  | `Db_not_found of Unix.process_status 
]

type round_error = [
  | `Not_found
  | `Cannot_retrieve_info of string
  | `Not_done
]

type form_error = [
  | `Expired of (string * string) list * float
  | `Wrong_session of (string * string) list
  | `Invalid_token of (string * string) list
  | `Missing_token of (string * string) list
  | `Many_tokens   of (string * string) list
  | `Wrong_content_type
]

type handler_error = [
  | `Key_not_found of string
]

type t = [ 
  | sql_error
  | process_error 
  | round_error 
  | handler_error 
  | form_error
]

let pp : t Misc.printer =
  fun fmt -> function
    | #Caqti_error.t as err -> Caqti_error.pp fmt err
    | _ -> failwith "not implemented yet"
