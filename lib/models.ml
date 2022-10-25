open Syntax

type ('a, 'b) request = Caqti_lwt.connection -> ('a, 'b) Lwt_result.t

let attach db_path (module Db : Caqti_lwt.CONNECTION) =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  Db.exec (string ->. unit @@ "attach database ? as other") db_path

let retrieve ~db_file ?db_attached req =
  let prefix = Filename.concat Options.benchpress_share_dir in
  let db_file_path = prefix db_file in
  let db_attached_path = Option.map prefix db_attached in
  let db_uri =
    Format.sprintf "sqlite3://%s" db_file_path
    |> Uri.of_string
  in
  match db_attached_path with
  | None -> Caqti_lwt.connect db_uri >>? req
  | Some db_attached_path -> 
      let con = Caqti_lwt.connect db_uri in
      let* _ = con >>? attach db_attached_path in 
      con >>? req 

module Fields = struct
  module Res = struct
    type t = Sat | Unsat | Unknown | Error

    let decode = function
      | "sat" -> Ok Sat
      | "unsat" -> Ok Unsat
      | "unknown" -> Ok Unknown
      | "error" -> Ok Error
      | _ -> Error "invalid result"

    let of_string res = Result.to_option (decode res)

    let t =
      let encode = function
        | Sat -> Ok "sat"
        | Unsat -> Ok "unsat"
        | Unknown -> Ok "unknown"
        | Error -> Ok "error"
      in
      Caqti_type.(custom ~encode ~decode string)
  end

  module Errcode = struct
    type t = Success | Failed of int
 
    let decode = function
      | 0 -> Ok Success
      | i -> Ok (Failed i)
 
    let of_string rc =
      match int_of_string_opt rc with
      | Some rc -> Result.to_option (decode rc)
      | None -> None

    let t =
      let encode = function
        | Success -> Ok 0
        | Failed i when i <> 0 -> Ok i
        | _ -> Error "invalid error code"
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

  let readdir ~dir = 
    File.readdir dir 
    |> List.map (fun filename -> {name=filename; version=""})
end

module Problem = struct
  open Fields

  type t = {
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

  let select =
    [%rapper
      get_many "\
        SELECT \
          prover as @string{prover_name}, \
          prover.version as @string{prover_version}, \
          file as @string{name}, \
          @Res{res}, \
          file_expect as @Res{expected_res}, \
          prover_res.timeout as @int{timeout}, \
          @octets{stdout}, \
          @octets{stderr}, \
          @Errcode{errcode}, \
          @float{rtime}, \
          CAST ((SELECT value FROM meta WHERE key = 'uuid') AS TEXT) \
            AS @string{uuid} \
        FROM prover_res, prover \
        WHERE \
          prover.name = prover_res.prover AND \
          (file = %string?{name} OR %string?{name} IS NULL) AND \
          (res = %Res?{res} OR %Res?{res} IS NULL) AND \
          (file_expect = %Res?{expected_res} OR %Res?{expected_res} \
            IS NULL) AND \
          (errcode = %Errcode?{errcode} OR %Errcode?{errcode} IS NULL) AND \
          (NOT %bool{only_diff} OR (res <> file_expect))\
      " record_out]

  let select_one =
    [%rapper
      get_one "\
        SELECT \
          prover as @string{prover_name}, \
          prover.version as @string{prover_version}, \
          file as @string{name}, \
          @Res{res}, \
          file_expect as @Res{expected_res}, \
          prover_res.timeout as @int{timeout}, \
          @octets{stdout}, \
          @octets{stderr}, \
          @Errcode{errcode}, \
          @float{rtime}, \
          CAST ((SELECT value FROM meta WHERE key = 'uuid') AS TEXT) \
            AS @string{uuid} \
        FROM prover_res, prover \
        WHERE \
          prover.name = prover_res.prover AND \
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
    rtime_1: float;
    rtime_2: float
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
          p1.rtime as @float{rtime_1}, \
          p2.rtime as @float{rtime_2} \
        FROM main.prover_res as p1, other.prover_res as p2 \
        WHERE p1.file = p2.file\
      " record_out]
end
