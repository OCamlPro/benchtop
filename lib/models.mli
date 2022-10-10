module type Model = sig
  type t 
  type caqti_t

  val to_sql : t -> (caqti_t, string) result
  val from_sql : caqti_t -> (t, string) result
end

type res = Sat | Unsat | Unknown | Error
type ext = Ae | Smt2 | Psmt2

module Problem : sig
  type t = private {
    prover: string;
    pb_name: string;
    pb_path: string;
    pb_ext: ext;
    res: res;
    expected_res: res;
    timeout: int;
    error_code: Unix.process_status;
    rtime: float 
  }
  type caqti_t = string * string * string * (string * int * int * float)

  include Model with type t := t and type caqti_t := caqti_t
end

module Round_summary : sig
  type t = private {
    uuid: string;
    running_at: Unix.tm;
    ctr_pbs : int;
    ctr_suc_pbs: int;
  } 
  type caqti_t = string * float * int * int

  include Model with type t := t and type caqti_t := caqti_t
end

module Prover : sig
  type t = private {
    name: string;
    version: string
  }
  type caqti_t = string * string

  include Model with type t := t and type caqti_t := caqti_t
end

val string_of_ext : ext -> string
val string_of_result : res -> string
val string_of_int : int -> string
val string_of_float : float -> string
val int_of_error_code : Unix.process_status -> int
