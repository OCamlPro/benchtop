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

module Res = struct
  type t =
    | Sat
    | Unsat
    | Unknown
    | Error
    | Timeout
    | Unexpected of string

  let decode = function
    | "sat" -> Ok Sat
    | "unsat" -> Ok Unsat
    | "unknown" -> Ok Unknown
    | "error" -> Ok Error
    | "timeout" -> Ok Timeout
    | str -> Ok (Unexpected str)

  let of_string res = Result.to_option (decode res)
  let to_string = function
    | Sat -> "sat"
    | Unsat -> "unsat"
    | Unknown -> "unknown"
    | Error -> "error"
    | Timeout -> "timeout"
    | Unexpected s -> s

  let pp fmt res = Format.fprintf fmt "%s" (to_string res)

  let t =
    let encode res = Ok (to_string res) in
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

  let to_string = function
    | Success -> "0"
    | Failed i when i <> 0 -> string_of_int i
    | _ -> failwith "invalid error code"

  let pp fmt rc = Format.fprintf fmt "%s" (to_string rc)

  let t =
    let encode = function
      | Success -> Ok 0
      | Failed i when i <> 0 -> Ok i
      | Failed _ -> Error "invalid error code"
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

module Kind_diff = struct
  type t = Improvement | Regression | Difference
  
  let decode = function
    | "improvement" -> Ok Improvement
    | "regression" -> Ok Regression
    | "difference" -> Ok Difference
    | _ as msg -> Error (Format.sprintf "Unknown kind of comparison '%s'" msg)
 
  let of_string kind = Result.to_option (decode kind)

  let t =
    let encode = function
      | Improvement -> Ok "improvement"
      | Regression -> Ok "regression"
      | Difference-> Ok "difference"
    in
    Caqti_type.(custom ~encode ~decode string)
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

  let of_binary_name binary = 
    let regexp = Str.regexp {|alt-ergo-\([a-zA-Z0-9_\-]+\)|} in
    let version = 
      if Str.string_match regexp binary 0 then
        Str.matched_group 1 binary
      else ""
    in
    {name="alt-ergo"; version} 
end

let pp_quote pp fmt = Format.fprintf fmt "'%a'" pp

module Problem = struct
  type t = {
    file: string;
    res: Res.t;
    file_expect: Res.t;
    timeout: int;
    stdout: string;
    stderr: string;
    errcode: Errcode.t;
    rtime: float;
  }

  let count ?(file="") ~res ~file_expect ~errcode ~only_diff 
    (module Db : Caqti_lwt.CONNECTION) =
    let open Caqti_type.Std in
    let open Caqti_request.Infix in
    Db.find (unit ->! int @@ 
      Format.asprintf "\
        SELECT \
          COUNT(*)
        FROM prover_res \
        WHERE \
          file LIKE '%%%s%%' AND \
          (res IN (%a) OR '%a' = '') AND \
          (file_expect IN (%a) OR '%a' = '') AND \
          (errcode IN (%a) OR '%a' = '') AND \
          (NOT %B OR (res <> file_expect))"
        file
        (Misc.pp_list (pp_quote Res.pp)) res (Misc.pp_list Res.pp) res
        (Misc.pp_list (pp_quote Res.pp)) file_expect (Misc.pp_list Res.pp) file_expect 
        (Misc.pp_list (pp_quote Errcode.pp)) errcode (Misc.pp_list Errcode.pp) errcode
        only_diff) () 

  let select, select_one =
    let function_out (file, (res, (file_expect, 
      (timeout, (stdout, (stderr, (errcode, rtime))))))) = {
        file;
        res;
        file_expect;
        timeout;
        stdout;
        stderr;
        errcode;
        rtime}
    in
    let open Caqti_type.Std in
    let open Caqti_request.Infix in
    let output = Caqti_type.
      (tup2 string 
        (tup2 Res.t 
          (tup2 Res.t 
            (tup2 int 
              (tup2 octets 
                (tup2 octets 
                  (tup2 Errcode.t float)))))))
    in
    ((fun ?(file="") ~res ~file_expect ~errcode ~only_diff ~page
      (module Db : Caqti_lwt.CONNECTION) -> 
      Db.collect_list (unit ->* output @@
      Format.asprintf 
      "SELECT \
        file, \
        res, \
        file_expect, \
        timeout, \
        stdout, \
        stderr, \
        errcode, \
        rtime \
      FROM prover_res \
      WHERE \
        file LIKE '%%%s%%' AND \
        (res IN (%a) OR '%a' = '') AND \
        (file_expect IN (%a) OR '%a' = '') AND \
        (errcode IN (%a) OR '%a' = '') AND \
        (NOT %B OR (res <> file_expect)) \
      LIMIT 50 OFFSET (50*%i)"
      file
      (Misc.pp_list (pp_quote Res.pp)) res (Misc.pp_list Res.pp) res
      (Misc.pp_list (pp_quote Res.pp)) file_expect (Misc.pp_list Res.pp) file_expect 
      (Misc.pp_list (pp_quote Errcode.pp)) errcode (Misc.pp_list Errcode.pp) errcode
      only_diff
      page) () >|? List.map function_out), 
    (fun ?(name = "NULL") (module Db : Caqti_lwt.CONNECTION) ->
      Db.find (unit ->! output @@
      Format.asprintf 
      "SELECT \
        file, \
        res, \
        file_expect, \
        timeout, \
        stdout, \
        stderr, \
        errcode, \
        rtime \
      FROM prover_res \
      WHERE \
        file LIKE '%%%s%%'"
      name) () >|? function_out))
end

module Round_summary = struct
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
  type t = Problem.t * Problem.t

  let count = 
    [%rapper
      get_one "\
        SELECT \
          @int{COUNT(*)}
        FROM main.prover_res as p1
        JOIN other.prover_res as p2 \
        ON p1.file = p2.file \
        WHERE \
          p1.file LIKE ('%' || %string{file} || '%') AND \
          ((((p1.res = p1.file_expect AND \
          p1.file_expect NOT IN ('error', 'unknown', 'timeout') AND \
          p2.res <> p2.file_expect) OR \
          ((ROUND(p1.rtime-p2.rtime, 0) <> 0 AND \
          p1.rtime < p2.rtime AND \
          NOT (p1.res = 'timeout' AND p2.res = 'timeout')) AND \
          %bool{show_rtime_reg})) AND \
          %Kind_diff{kind_diff} = 'improvement') \
          OR \
          (((p2.res = p2.file_expect AND \
          p2.file_expect NOT IN ('error', 'unknown', 'timeout') AND \
          p1.res <> p1.file_expect) OR \
          ((ROUND(p1.rtime-p2.rtime, 0) <> 0 AND \
          p1.rtime < p2.rtime AND \
          NOT (p1.res = 'timeout' AND p2.res = 'timeout')) AND \
          %bool{show_rtime_reg})) AND \
          %Kind_diff{kind_diff} = 'regression') \
          OR \
          (((p1.res <> p2.res OR \
          p1.file_expect <> p2.file_expect OR \
          p1.errcode <> p2.errcode) OR \
          ((ROUND(p1.rtime-p2.rtime, 0) <> 0 AND \
          NOT (p1.res = 'timeout' AND p2.res = 'timeout')) OR \
          %bool{show_rtime_reg})) AND \
          %Kind_diff{kind_diff} = 'difference'))
      "]

  let select =
    [%rapper get_many "\
      SELECT \
        p1.file AS @string{file1}, \
        p1.res AS @Res{res1}, \
        p1.file_expect @Res{file_expect1}, \
        p1.timeout @int{timeout1}, \
        p1.stdout AS @octets{stdout1}, \
        p1.stderr AS @octets{stderr1}, \
        p1.errcode AS @Errcode{errcode1}, \
        p1.rtime AS @float{rtime1}, \
        p2.file AS @string{file2}, \
        p2.res AS @Res{res2}, \
        p2.file_expect @Res{file_expect2}, \
        p2.timeout @int{timeout2}, \
        p2.stdout AS @octets{stdout2}, \
        p2.stderr AS @octets{stderr2}, \
        p2.errcode AS @Errcode{errcode2}, \
        p2.rtime AS @float{rtime2} \
      FROM main.prover_res AS p1 \
      JOIN other.prover_res AS p2 \
      ON p1.file = p2.file \
      WHERE \
          p1.file LIKE ('%' || %string{file} || '%') AND \
          ((((p1.res = p1.file_expect AND \
          p1.file_expect NOT IN ('error', 'unknown', 'timeout') AND \
          p2.res <> p2.file_expect) OR \
          ((ROUND(p1.rtime-p2.rtime, 0) <> 0 AND \
          p1.rtime < p2.rtime AND \
          NOT (p1.res = 'timeout' AND p2.res = 'timeout')) AND \
          %bool{show_rtime_reg})) AND \
          %Kind_diff{kind_diff} = 'improvement') \
          OR \
          (((p2.res = p2.file_expect AND \
          p2.file_expect NOT IN ('error', 'unknown', 'timeout') AND \
          p1.res <> p1.file_expect) OR \
          ((ROUND(p1.rtime-p2.rtime, 0) <> 0 AND \
          p1.rtime < p2.rtime AND \
          NOT (p1.res = 'timeout' AND p2.res = 'timeout')) AND \
          %bool{show_rtime_reg})) AND \
          %Kind_diff{kind_diff} = 'regression') \
          OR \
          (((p1.res <> p2.res OR \
          p1.file_expect <> p2.file_expect OR \
          p1.errcode <> p2.errcode) OR \
          ((ROUND(p1.rtime-p2.rtime, 0) <> 0 AND \
          NOT (p1.res = 'timeout' AND p2.res = 'timeout')) OR \
          %bool{show_rtime_reg})) AND \
          %Kind_diff{kind_diff} = 'difference'))
      ORDER BY CASE WHEN
        p1.res <> p2.res OR \
        p1.file_expect <> p2.file_expect OR \
        p1.errcode <> p2.errcode THEN 0
        ELSE 1 END
      LIMIT 50 OFFSET (50 * %int{page})\
    " function_out] (fun 
      ~file1 ~res1 ~file_expect1 ~timeout1 ~stdout1 ~stderr1 ~errcode1 ~rtime1
      ~file2 ~res2 ~file_expect2 ~timeout2 ~stdout2 ~stderr2 ~errcode2 ~rtime2 
      -> Problem.({
        file=file1; 
        res=res1; 
        file_expect=file_expect1;
        timeout=timeout1;
        stdout=stdout1; 
        stderr=stderr1;
        errcode=errcode1;
        rtime=rtime1},
        {file=file2; 
        res=res2; 
        file_expect=file_expect2; 
        timeout=timeout2;
        stdout=stdout2; 
        stderr=stderr2;
        errcode=errcode2;
        rtime=rtime2}
      ))
end
