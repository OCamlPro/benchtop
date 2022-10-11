module type Model = sig
  type t 
  type caqti_t

  val to_sql : t -> (caqti_t, string) result
  val from_sql : caqti_t -> (t, string) result
end

type res = Sat | Unsat | Unknown | Error
type ext = Ae | Smt2 | Psmt2

let int_of_error_code = 
  let open Unix in function
  | WEXITED code | WSIGNALED code | WSTOPPED code -> code

let error_code_of_int i =
  Unix.WEXITED i

let string_of_result res =
  match res with
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
  | Error -> "error"

let string_of_ext ext =
  match ext with
  | Ae -> "ae"
  | Smt2 -> "smt2"
  | Psmt2 -> "psmt2"

let result_of_string = function
  | "sat" -> Sat
  | "unsat" -> Unsat
  | "unknown" -> Unknown
  | "error" -> Error
  | str -> 
    failwith (Format.sprintf "Unexpected result: %s" str)
      
let ext_of_string = function
  | ".ae" -> Ae
  | ".smt2" -> Smt2
  | ".psmt2" -> Psmt2
  | str -> 
    failwith (Format.sprintf "Unexpected extension: %s" str)

module Problem = struct
  type t = {
    prover: string;
    name: string;
    ext: ext;
    res: res;
    expected_res: res;
    timeout: int;
    stdout: string;
    stderr: string;
    error_code: Unix.process_status;
    rtime: float 
  }
  type caqti_t = string * string * string * 
    (string * int * string * (string * int * float))


  let to_sql {prover; name; res; expected_res; 
    timeout; stdout; stderr; error_code; rtime; _} = 
    let file_path = name in 
    let res = string_of_result res in 
    let file_expect = string_of_result expected_res in
    let errcode = int_of_error_code error_code in 
    Ok (prover, file_path, res, 
      (file_expect, timeout, stdout, (stderr, errcode, rtime)))
  
  let from_sql (prover, file, res, 
    (file_expect, timeout, stdout, (stderr, errcode, rtime))) =
    let name = file in
    let ext = Filename.extension file |> ext_of_string in
    let res = result_of_string res in
    let expected_res = result_of_string file_expect in
    let error_code = error_code_of_int errcode in
    Ok {prover; name; ext; res; expected_res; 
      timeout; stdout; stderr; error_code; rtime} 
end

module Round_summary = struct
  type t = {
    uuid: string;
    running_at: Unix.tm;
    ctr_pbs : int;
    ctr_suc_pbs: int;
  }
  type caqti_t = string * float * int * int

  let to_sql {uuid; running_at; ctr_pbs; ctr_suc_pbs} = 
    let running_at, _ = Unix.mktime running_at in 
    Ok (uuid, running_at, ctr_pbs, ctr_suc_pbs)

  let from_sql (uuid, running_at, ctr_pbs, ctr_suc_pbs) = 
    let running_at = Unix.localtime running_at in
    Ok {uuid; running_at; ctr_pbs; ctr_suc_pbs}
end

module Prover = struct
  type t = {
    name: string;
    version: string
  }
  type caqti_t = string * string
  
  let to_sql {name; version} = Ok (name, version) 
  let from_sql (name, version) = Ok {name; version}
end

let string_of_int = Format.sprintf "%i"
let string_of_float = Format.sprintf "%f"


