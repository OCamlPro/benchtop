type 'a answer = ('a, Caqti_error.t) Lwt_result.t
type 'a request = (module Caqti_lwt.CONNECTION) -> 'a answer

let exec ~db_file req = 
  let db_uri = 
    "sqlite3://" ^ Filename.concat Options.benchpress_share_dir db_file 
    |> Uri.of_string
  in
  Lwt_result.bind (Caqti_lwt.connect db_uri) req

let debug ans = 
  Lwt_result.bind_lwt_error ans (fun err -> 
    let msg = Format.asprintf "%a" Caqti_error.pp err in
    Dream.error (fun log -> log "%s" msg);
    Lwt.return msg
  )

let (@) req1 req2 db =
  Lwt_result.bind (req1 db) (fun res1 ->
    match%lwt req2 db with
    | Ok res2 -> Lwt_result.return (res1, res2)
    | Error err -> Lwt_result.fail err)

module Query_strings = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let select_problems =
    let open Models.Problem in
    let rep_ty = 
      Caqti_type.(tup4 string string string (tup4 string int int float))
    in
    let problem = custom ~encode:to_sql ~decode:from_sql rep_ty in
    unit ->* problem @@ 
    {eos|
      SELECT 
        prover, file, res, 
        file_expect, timeout, errcode, rtime 
      FROM prover_res 
    |eos}

  let round_summary = 
    let open Models.Round_summary in
    let rep_ty = Caqti_type.(tup4 string float int int) in 
    let round_sum = custom ~encode:to_sql ~decode:from_sql rep_ty in
    unit ->! round_sum @@
    {eos|
      SELECT
        CAST ((SELECT value FROM meta WHERE key = "uuid") AS TEXT) 
          AS uuid, 
        CAST ((SELECT value FROM meta WHERE key = "timestamp") AS REAL) 
          AS timestamp,
        (SELECT COUNT(file) FROM prover_res) 
          AS ctr_pbs,
        (SELECT COUNT(file) FROM prover_res WHERE res = file_expect) 
          AS ctr_suc_pbs
    |eos}

  let provers = 
    let open Models.Prover in
    let rep_ty = Caqti_type.(tup2 string string) in
    let prover = custom ~encode:to_sql ~decode:from_sql rep_ty in
    unit ->! prover @@
    {eos|
      SELECT 
        name, version
      FROM prover
    |eos}

  let set_wal = 
    unit ->! int @@
    "PRAGMA busy_timeout = 5000"
end

let round_summary (module Db : Caqti_lwt.CONNECTION) =
  Db.find Query_strings.round_summary ()

let select_problems (module Db : Caqti_lwt.CONNECTION) =
  Db.collect_list Query_strings.select_problems ()

let provers (module Db : Caqti_lwt.CONNECTION) = 
  Db.collect_list Query_strings.provers () 

let set_wal (module Db : Caqti_lwt.CONNECTION) =
  Db.find Query_strings.set_wal ()


