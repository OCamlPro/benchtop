type 'a answer = ('a, Error.t) Lwt_result.t
type 'a request = Caqti_lwt.connection -> 'a answer

let retrieve ~db_file req =
  let db_absolute_path =
    Filename.concat Options.benchpress_share_dir db_file
  in
  let db_uri =
    Format.sprintf "sqlite3://%s" db_absolute_path
    |> Uri.of_string
  in
  let ans = Lwt_result.bind (Caqti_lwt.connect db_uri) req in
  Lwt_result.bind_lwt_error ans (fun err ->
    Dream.error (fun log -> log "%a" Error.pp err);
    Lwt.return err
  )

module Fields = struct
  module Res = struct
    type t = Sat | Unsat | Unknown | Error

    let t =
      let encode = function
        | Sat -> Ok "sat"
        | Unsat -> Ok "unsat"
        | Unknown -> Ok "unknown"
        | Error -> Ok "error"
      in
      let decode = function
        | "sat" -> Ok Sat
        | "unsat" -> Ok Unsat
        | "unknown" -> Ok Unknown
        | "error" -> Ok Error
        | _ -> Error "invalid result"
      in
      Caqti_type.(custom ~encode ~decode string)
  end

  module Ext = struct
    type t = Ae | Smt2 | Psmt2

    let t =
      let encode = function
        | Ae -> Ok "ae"
        | Smt2 -> Ok "smt2"
        | Psmt2 -> Ok "psmt2"
      in
      let decode = function
        | "ae" -> Ok Ae
        | "smt2" -> Ok Smt2
        | "psmt2" -> Ok Psmt2
        | _ -> Error "invalid extension of problem"
      in
      Caqti_type.(custom ~encode ~decode string)
  end

  module Errcode = struct
    type t = Success | Failed of int 
 
    let t =
      let encode = function
        | Success -> Ok 0
        | Failed i when i <> 0 -> Ok i
        | _ -> Error "invalid error code"
      in
      let decode = function
        | 0 -> Ok Success
        | i -> Ok (Failed i)
      in
      Caqti_type.(custom ~encode ~decode int)
  end

  module Time = struct
    type t = Unix.tm

    let t =
      let encode tm =
        let tm, _ = Unix.mktime tm in
        Ok tm
      in
      let decode tm =
        Ok (Unix.localtime tm)
      in
      Caqti_type.(custom ~encode ~decode float)
  end
end

module Problem = struct
  open Fields

  type t = {
    name: string;
    prover: string;
    res: Res.t;
    expected_res: Res.t;
    timeout: int;
    stdout: string;
    stderr: string;
    errcode: Errcode.t;
    rtime: float
  }

  let select =
    [%rapper
      get_many "\
        SELECT \
          @string{prover}, \
          file as @string{name}, \
          @Res{res}, \
          file_expect as @Res{expected_res}, \
          @int{timeout}, \
          @octets{stdout}, \
          @octets{stderr}, \
          @Errcode{errcode}, \
          @float{rtime} \
        FROM prover_res \
        WHERE \
          (file = %string?{name} OR %string?{name} IS NULL) AND \
          (res = %Res?{res} OR %Res?{res} IS NULL) AND \
          (file_expect = %Res?{expected_res} OR %Res?{expected_res} \
            IS NULL) AND \
          (errcode = %Errcode?{errcode} OR %Errcode?{errcode} IS NULL) \
      " record_out]

  let select_one =
    [%rapper
      get_one "\
        SELECT \
          @string{prover}, \
          file as @string{name}, \
          @Res{res}, \
          file_expect as @Res{expected_res}, \
          @int{timeout}, \
          @octets{stdout}, \
          @octets{stderr}, \
          @Errcode{errcode}, \
          @float{rtime} \
        FROM prover_res \
        WHERE \
          file = %string{name}\
      " record_out]
end

module Round_summary = struct
  open Fields 

  type t = {
    uuid: string;
    running_at: Time.t;
    ctr_pbs : int;
    ctr_suc_pbs: int
  }

  let retrieve =
    [%rapper
      get_one "\
        SELECT \
          CAST ((SELECT value FROM meta WHERE key = 'uuid') AS TEXT) \
            AS @string{uuid},\
          CAST ((SELECT value FROM meta WHERE key = 'timestamp') AS REAL) \
            AS @Time{running_at},\
          (SELECT COUNT(file) FROM prover_res) \
            AS @int{ctr_pbs},\
          (SELECT COUNT(file) FROM prover_res WHERE res = file_expect) \
            AS @int{ctr_suc_pbs}\
      " record_out]
end

module Prover = struct
  type t = {
    name: string;
    version: string
  }

  let select =
    [%rapper
      get_many "\
        SELECT \
          @string{name}, @string{version} \
        FROM prover \
        WHERE \
          (name = %string?{name} OR %string?{name} IS NULL) AND \
          (version = %string?{version} OR %string?{version} IS NULL)\
      " record_out]
  
  let select_one =
    [%rapper 
      get_one "\
        SELECT \
          @string{name}, @string{version} \
        FROM prover \
        WHERE \
          name = %string{name} AND \
          (version = %string?{version} OR %string?{version} IS NULL)\
      " record_out]
end

module Problem_diff = struct
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
    rtime_diff: float
  }

  let select =
    [%rapper
      get_many "\
        SELECT \
          p1.file as @string{name}, \
          p1.prover as @string{prover_1}, \
          p2.prover as @string{prover_2}, \
          p1.res as @Res{res_1}, \
          p2.res as @Res{res_2}, \
          p1.file_expect as @Res{expected_res_1}, \
          p2.file_expect as @Res{expected_res_2}, \
          p1.errcode as @Errcode{errcode_1}, \
          p2.errcode as @Errcode{errcode_2}, \
          ROUND(p1.rtime - p2.rtime, 4) as @float{rtime_diff} \
        FROM main.prover_res as p1, other.prover_res as p2 \
        WHERE p1.file = p2.file\
      " record_out]
end
