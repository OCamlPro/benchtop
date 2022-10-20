type sql = Caqti_error.t 

type process = [
  | `Is_running
  | `Stopped of Unix.process_status
  | `Db_not_found
]

type round = [
  | sql
  | process
  | `Round_not_found
  | `Cannot_retrieve_info of string
  | `Not_done
]

type form = [
  | `Expired of (string * string) list * float
  | `Wrong_session of (string * string) list
  | `Invalid_token of (string * string) list
  | `Missing_token of (string * string) list
  | `Many_tokens   of (string * string) list
  | `Wrong_content_type
]

type param = [
  | form
  | `Key_not_found of string
]

type t = [
  | sql
  | process
  | round
  | param
  | form
]

let pp =
  fun fmt -> function
    | #Caqti_error.t as err -> Caqti_error.pp fmt err
    | _ -> failwith "not implemented yet"
