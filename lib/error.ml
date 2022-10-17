type process_error = [
  | `Stopped of Unix.process_status
  | `Db_not_found of Unix.process_status 
]

type round_error = [
  | `Not_found
  | `Cannot_retrieve_info of string
  | `Not_done
]

type t = [ Caqti_error.t | process_error | round_error ]

let pp : t Misc.printer =
  fun fmt -> function
    | #Caqti_error.t as err -> Caqti_error.pp fmt err
    | _ -> failwith "not implemented yet"
