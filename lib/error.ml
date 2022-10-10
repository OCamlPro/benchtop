type process_error = [
  | `Stopped of Unix.process_status
  | `Db_not_found of Unix.process_status 
]

type round_error = [
  | `Cannot_retrieve_info of string
  | `Not_done
]

type t = [ Caqti_error.t | process_error | round_error ]


