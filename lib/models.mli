type ('a, 'b) request = Caqti_lwt.connection -> ('a, 'b) Lwt_result.t

val retrieve :
  db_file:string ->
  ?db_attached:string ->
  ('a, [> Caqti_error.t] as 'b) request ->
  ('a, 'b) Lwt_result.t

module Fields : sig 
  module Res : sig 
    type t = private Sat | Unsat | Unknown | Error

    val of_string : string -> t option
    include Rapper.CUSTOM with type t := t
  end

  module Errcode : sig
    type t = private Success | Failed of int 

    val of_string : string -> t option
    include Rapper.CUSTOM with type t := t
  end

  module Time : sig
    type t = Unix.tm
    include Rapper.CUSTOM with type t := t
  end
end

module Prover : sig
  type t = private {
    name: string;
    version: string
  }

  val select : 
    name: string option ->
    version: string option ->
    (t list, [> Error.sql]) request

  val select_one : 
    name: string ->
    version: string option ->
    (t, [> Error.sql]) request

  val readdir :
    dir: string ->
    t list
end

module Problem : sig
  open Fields

  type t = private {
    prover_name: string;
    prover_version: string;
    name: string;
    res: Res.t;
    expected_res: Res.t;
    timeout: int;
    stdout: string;
    stderr: string;
    errcode: Errcode.t;
    rtime: float;
    uuid: string
  }

  val count :
    name:string option ->
    res:Res.t option ->
    expected_res:Res.t option ->
    errcode:Errcode.t option ->
    only_diff:bool -> 
    (int, [> Error.sql]) request
 
  val select :
    name:string option ->
    res:Res.t option ->
    expected_res:Res.t option ->
    errcode:Errcode.t option ->
    only_diff:bool ->
    offset:int ->
    (t list, [> Error.sql]) request

  val select_one :
    name:string ->
    (t, [> Error.sql]) request
end

module Round_summary : sig
  open Fields

  type t = private {
    uuid: string;
    running_at: Time.t;
    ctr_pbs : int;
    ctr_suc_pbs: int
  }

  val retrieve : unit -> (t, [> Error.sql]) request
end

module Problem_diff : sig
  open Fields

  type t = {
    name: string;
    prover_1: string;
    prover_2: string;
    res_1: Res.t;
    res_2: Res.t;
    expected_res_1: Res.t;
    expected_res_2: Res.t;
    errcode_1 : Errcode.t;
    errcode_2: Errcode.t;
    rtime_1: float;
    rtime_2: float
  }

  val count : unit -> (int, [> Error.sql]) request

  val select :
    offset:int ->
    (t list, [> Error.sql]) request
end 
