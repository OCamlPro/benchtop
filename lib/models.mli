type ('a, 'b) request = Caqti_lwt.connection -> ('a, 'b) Lwt_result.t

val retrieve :
  db_file:string ->
  ?db_attached:string ->
  ('a, ([> Caqti_error.t ] as 'b)) request ->
  ('a, 'b) Lwt_result.t

module Res : sig
  type t = private
    | Sat
    | Unsat
    | Unknown
    | Error
    | Timeout
    | Unexpected of string

  val of_string : string -> t option
  val to_string : t -> string

  include Rapper.CUSTOM with type t := t
end

module Errcode : sig
  type t = private Success | Failed of int

  val of_string : string -> t option

  include Rapper.CUSTOM with type t := t
end

(* TODO: replace this type by the built-in date in Caqti. *)
module Time : sig
  type t = Unix.tm

  include Rapper.CUSTOM with type t := t

  val pp : t Fmt.t
  val show : t -> string
end

module Kind_diff : sig
  type t = Improvement | Regression | Difference

  val of_string : string -> t option

  include Rapper.CUSTOM with type t := t
end

module Prover : sig
  type t = private { name : string; version : string }

  val select :
    name:string option ->
    version:string option ->
    (t list, [> Error.sql ]) request

  val select_one :
    name:string -> version:string option -> (t, [> Error.sql ]) request

  val readdir : dir:string -> t list
  val of_binary_name : string -> t
  val pp : t Fmt.t
  val show : t -> string
end

module Problem : sig
  type t = private {
    file : string;
    res : Res.t;
    file_expect : Res.t;
    timeout : int;
    stdout : string;
    stderr : string;
    errcode : Errcode.t;
    rtime : float;
  }

  val count :
    ?file:string ->
    res:Res.t list ->
    file_expect:Res.t list ->
    errcode:Errcode.t list ->
    only_diff:bool ->
    (int, [> Error.sql ]) request

  val select :
    ?file:string ->
    res:Res.t list ->
    file_expect:Res.t list ->
    errcode:Errcode.t list ->
    only_diff:bool ->
    page:int ->
    (t list, [> Error.sql ]) request

  val select_one : ?name:string -> (t, [> Error.sql ]) request
end

module Round_summary : sig
  type t = private {
    uuid : string;
    running_at : Time.t;
    ctr_pbs : int;
    ctr_suc_pbs : int;
  }

  val retrieve : unit -> (t, [> Error.sql ]) request
end

module Problem_diff : sig
  type t = Problem.t * Problem.t

  val count :
    file:string ->
    threshold:float ->
    show_rtime_reg:bool ->
    kind_diff:Kind_diff.t ->
    (int, [> Error.sql ]) request

  val select :
    file:string ->
    threshold:float ->
    show_rtime_reg:bool ->
    kind_diff:Kind_diff.t ->
    page:int ->
    (t list, [> Error.sql ]) request
end
