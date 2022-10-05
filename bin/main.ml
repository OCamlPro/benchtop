open Benchtop

let benchpress_share_dir = "/home/tiky/.local/share/benchpress"
let configs_dir = List.hd Location.Sites.configs
let tests_dir = List.hd Location.Sites.tests

let string_of_int = Format.sprintf "%i"
let string_of_float = Format.sprintf "%f"

let int_of_error_code = 
  let open Unix in function
  | WEXITED code | WSIGNALED code | WSTOPPED code -> code

let error_code_of_int i =
  Unix.WEXITED i

type res = Sat | Unsat | Unknown | Error
type ext = Ae | Smt2 | Psmt2

module File = struct
  let cat fmt fl =
    let ch = open_in fl in
    try while true do
      let s = input_line ch in
      Format.fprintf fmt "%s@\n" s
    done
    with End_of_file ->
    Format.fprintf fmt "@."

  let readdir ?ext_filter path =     
    let apply_filter = 
      match ext_filter with
      | Some filter -> 
          let test file = filter @@ Filename.extension file in
          List.filter test
      | None -> fun x -> x
    in
    Sys.readdir path |> Array.to_list |> apply_filter 

  let read_all ch =
    let buf = Buffer.create 113 in
    try
      while true do
        Buffer.add_channel buf ch 30
      done;
      assert false
    with End_of_file ->
      Buffer.contents buf
end

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

  val decode : string * (string * (string * (string * (int * (int * float))))) 
    -> (t, _) result
  val encode : t -> 
    (string * (string * (string * (string * (int * (int * float))))), _) result 

  val is_successful : t -> bool
end = struct
  type t = {
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

  let decode (prover, (file, (res, (file_expect, 
    (timeout, (errcode, rtime)))))) = 
    let pb_name = Filename.basename file |> Filename.chop_extension in
    let pb_path = Filename.dirname file in
    let pb_ext = Filename.extension file |> ext_of_string in
    let res = result_of_string res in
    let expected_res = result_of_string file_expect in
    let error_code = error_code_of_int errcode in
    Ok {prover; pb_name; pb_path; pb_ext; res; expected_res; 
      timeout; error_code; rtime} 

  let encode {prover; pb_name; pb_path; pb_ext; res; expected_res; 
    timeout; error_code; rtime} = 
    let file_path = Filename.concat pb_path (pb_name ^ string_of_ext pb_ext) in
    let res = string_of_result res in 
    let file_expect = string_of_result expected_res in
    let errcode = int_of_error_code error_code in 
    Ok (prover, (file_path, (res, (file_expect, (timeout, (errcode, rtime))))))

  let is_successful pb =
    match (pb.res, pb.expected_res) with
    | Sat, Sat | Unsat, Unsat 
    | Unknown, Unknown | Error, Error -> true
    | _ -> false 
end

module Round_info : sig
  type t = private {
    provers: (string * string) list;
    uuid: string;
    running_at: Unix.tm;
    ctr_pbs : int;
    ctr_suc_pbs: int;
  }

  val make: provers: (string * string) list -> uuid: string -> 
    timestamp: float -> ctr_pbs: int -> ctr_suc_pbs: int -> t
end = struct
  type t = {
    provers: (string * string) list;
    uuid: string;
    running_at: Unix.tm;
    ctr_pbs : int;
    ctr_suc_pbs: int;
  }

  let make ~provers ~uuid ~timestamp ~ctr_pbs ~ctr_suc_pbs =
    let running_at = Unix.localtime timestamp in
    {provers; uuid; running_at; ctr_pbs; ctr_suc_pbs}
end

module Request : sig 
  type 'a request = (module Caqti_lwt.CONNECTION) -> 
    ('a, string) Lwt_result.t

  val round_info : Round_info.t request

  val select_problems : Problem.t list request

  val set_wal : unit request
end = struct
  type 'a request = (module Caqti_lwt.CONNECTION) -> 
    ('a, string) Lwt_result.t

  module Query_strings = struct
    open Caqti_request.Infix
    open Caqti_type.Std
 
    let select_problems =
      let open Problem in
      let rep_ty = 
        Caqti_type.
        (tup2 string 
          (tup2 string 
            (tup2 string 
              (tup2 string 
                (tup2 int 
                  (tup2 int float))))))  
      in
      let problem = custom ~encode ~decode rep_ty in
      unit ->* problem @@ 
      {eos|
        SELECT 
          prover, file, res, 
          file_expect, timeout, errcode, rtime 
        FROM prover_res 
      |eos}

    let meta_info = 
      let rep_ty = Caqti_type.(tup4 string float int int) in 
      unit ->! rep_ty @@
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
      unit ->! Caqti_type.(tup2 string string) @@
      {eos|
        SELECT 
          name, version
        FROM prover
      |eos}

    let count_tests =
      unit ->! int @@
      {eos|
        SELECT
          COUNT(file)
        FROM prover_res
      |eos}

    let count_successful_tests =
      unit ->! int @@
      {eos|
        SELECT
          COUNT(file)
        FROM prover_res
        WHERE res = file_expect
      |eos}

    let set_wal = 
      unit ->. unit @@
      "PRAGMA journal_mode=WAL"
  end

  let round_info (module Db : Caqti_lwt.CONNECTION) =
    Lwt_result.bind_lwt_error (
      Lwt_result.bind (Db.find Query_strings.meta_info ())
      (fun (uuid, timestamp, ctr_pbs, ctr_suc_pbs) ->
        Lwt_result.bind_result (Db.collect_list Query_strings.provers ())
        (fun provers ->
          let round_info = Round_info.make 
            ~provers ~uuid ~timestamp ~ctr_pbs ~ctr_suc_pbs
          in 
          Ok round_info)))
      (fun err ->
        Dream.log "%a" Caqti_error.pp err;
        Lwt.return "Cannot get the round informations")

  let select_problems (module Db : Caqti_lwt.CONNECTION) =
    Lwt_result.bind_lwt_error ( 
      Db.collect_list Query_strings.select_problems ())
      (fun err -> 
        Dream.log "%a" Caqti_error.pp err;
        Lwt.return "Cannot get the list of problems")

  let set_wal (module Db : Caqti_lwt.CONNECTION) =
    Lwt_result.bind_lwt_error (
      Db.exec Query_strings.set_wal ())
      (fun err ->
        Dream.log "%a" Caqti_error.pp err;
        Lwt.return "Cannot set WAL mode on")
end

let connect db_file = 
  let db_uri = 
    "sqlite3://" ^ Filename.concat benchpress_share_dir db_file 
    |> Uri.of_string
  in
  Lwt_result.bind_lwt_error (Caqti_lwt.connect db_uri) (fun err ->
    Lwt.return @@ Format.asprintf "%a" Caqti_error.pp err
  )

module Round : sig 
  type t
  
  type data = {
    db_file: string;
    proc: Lwt_process.process_none option;
    info: Round_info.t
  }

  type status =  
    | Pending     
    | Running of data 
    | Done of data 
    | Cancelled 

  val make : unit -> t
  val resurect : db_file: string -> (t, string) Lwt_result.t
  val run : t -> (t, string) Lwt_result.t
  val update : t -> (t, string) Lwt_result.t
  val cancel : t -> t 
  val compare : t -> t -> int

  val config: t -> string
  val date : t -> Unix.tm
  val status : t -> status
  val problems : t -> (Problem.t list, string) Lwt_result.t 
end = struct 
  type data = {
    db_file: string;
    proc: Lwt_process.process_none option;
    info: Round_info.t
  }

  type status = 
    | Pending 
    | Running of data
    | Done of data
    | Cancelled 

  type t = {
    cmd: Lwt_process.command;
    config: string;
    pending_at: Unix.tm;
    status: status
  }

  let config round = round.config
  let status round = round.status
  let date round = 
    match status round with 
    | Pending -> round.pending_at
    | Cancelled -> round.pending_at
    | Running {info; _} | Done {info; _} -> info.running_at 
  
  let make () =
    let config_path = "config.sexp" in {
      cmd = ("benchpress", [|"benchpress"; "run"; "-c"; 
        "lib/config.sexp"; "-p"; "alt-ergo"; "lib/tests"|]);
      config = "default";
      pending_at = Unix.time () |> Unix.localtime;
      status = Pending 
  }

  type res = Found_db of string | Dead of Unix.process_status

  let rec wait_db_file =
    let is_db file = 
      String.equal (Filename.extension file) ".sqlite"
    in
    fun inotify ->
    match%lwt Lwt_inotify.read inotify with
      | (_, [Inotify.Create], _, Some file) when is_db file -> 
          Dream.log "%s" file;
          Lwt.return (Found_db file)
      | _ -> wait_db_file inotify

  let wait_terminate proc = 
    let%lwt rc = proc#status in
    Lwt.return (Dead rc)

  let retrieve_info db_file =
    Lwt_result.bind (connect db_file) Request.round_info

  let resurect ~db_file = 
    let round = make () in
    Lwt_result.bind_lwt (retrieve_info db_file) (fun info ->
      Lwt.return { round with status = Done {db_file; info; proc = None} })

  let create_process =
    let create_temp_file () =
      let file = Filename.temp_file "benchpress" "toto" in
      Unix.openfile file [O_RDWR] 0o640 
    in
    fun cmd ->
    let stdout_fd = create_temp_file () in 
    let stderr_fd = create_temp_file () in 
    let proc = Lwt_process.open_process_none ~stdout:(`FD_copy stdout_fd) 
      ~stderr:(`FD_copy stderr_fd) cmd in
    (proc, 
      Unix.in_channel_of_descr stdout_fd, 
      Unix.in_channel_of_descr stderr_fd)

  let run round = 
    match status round with
    | Pending -> begin
      let%lwt inotify = Lwt_inotify.create () in
      let%lwt _ = Lwt_inotify.add_watch inotify benchpress_share_dir 
        [Inotify.S_Create] in
      Dream.log "Ready to benchpress@.";
      let proc, stdout_ch, stderr_ch = create_process round.cmd in
      Dream.log "Running benchpress@.";
      match%lwt Lwt.choose [wait_db_file inotify; wait_terminate proc] with
      | Found_db db_file -> 
          Dream.log "Found db: %s@." db_file;
          Lwt_result.bind_lwt (retrieve_info db_file) (fun info ->
            let status = Running { db_file; info; proc = None } in
            Lwt.return {round with status}
          )
      | Dead rc ->
          Dream.log "%s" (File.read_all stdout_ch);
          (match rc with
          | Unix.WEXITED i -> Dream.log "WEXITED: %i" i 
          | Unix.WSIGNALED i -> Dream.log "WSIGNALED: %i" i 
          | Unix.WSTOPPED i -> Dream.log "WSTOPPED: %i" i);
          Lwt_result.return { round with status = Cancelled }
      end
    | Running _ | Done _ | Cancelled -> 
        Lwt_result.return round 

  let update round = 
    match status round with
    | Running ({db_file; proc = Some proc; _ } as data) -> 
        Lwt_result.bind_result (retrieve_info db_file) (fun info ->
          match proc#state with
          | Lwt_process.Running -> 
              Ok {round with status = Running {data with info}}
          | Lwt_process.Exited rc -> 
              match rc with
              | WEXITED 0 -> 
                  Ok {round with status = Done {data with info}}
              | WEXITED i | WSIGNALED i | WSTOPPED i ->
                  Error (Format.sprintf "Stopped with the code %i" i))
    | Done _ | Pending | Cancelled -> Lwt_result.return round
    | Running _ -> failwith "Running round with any processus attached"

  let compare round1 round2 = 
    let date1 = Unix.mktime (date round1) |> fst in
    let date2 = Unix.mktime (date round2) |> fst in
    Float.compare date2 date1

  let cancel round =  
    match round.status with
    | Pending -> { round with status = Cancelled }    
    | Running {proc = Some proc; _} -> begin 
        proc#terminate;
        ignore proc#close;
        { round with status = Cancelled } 
      end
    | Running _ | Done _ | Cancelled -> round

  let problems round =
    match status round with
    | Running {db_file; info; _} | Done {db_file; info; _} -> 
      Lwt_result.bind (connect db_file) (Request.select_problems) 
    | Pending | Cancelled -> Lwt_result.fail "No problem list available."
end 

module Views : sig
  type view = Dream.request -> string 

  val render_404_not_found : view
  val render_rounds_list : Round.t list -> view
  val render_round_detail : Problem.t list -> view
  val render_problem_detail : Problem.t -> view
end = struct
  type view = Dream.request -> string 
  
  let html_to_string html = 
    Format.asprintf "%a@." (Tyxml.Html.pp ~indent:true ()) html

  let format_date (tm : Unix.tm) = 
    Format.sprintf "%02i/%02i/%04i %02i:%02i:%02i"
      tm.tm_mday tm.tm_mon tm.tm_year
      tm.tm_hour tm.tm_min tm.tm_sec 

  let sprintf_list pp lst =
    let buf = Buffer.create 200 in
    let fmt = Format.formatter_of_buffer buf in
    let pp_sep fmt () = Format.fprintf fmt ", " in 
    Format.pp_print_list ~pp_sep pp fmt lst;
    Buffer.to_seq buf |> String.of_seq

  let navbar content = 
    let open Tyxml in
    [%html
      "<header class=\"navbar bg-light sticky-top\">\
        <div class=\"container-fluid\">\
          <a class=\"navbar-brand text-primary\" href=\"#\">Benchtop</a>"
          content  
        "</div>\
      </header>"
    ]

  let page_layout ~subtitle ?(hcontent=[]) ?(fcontent=[]) content =
    let open Tyxml.Html in
    let str = Format.sprintf "Benchtop -- %s" subtitle in
    (* TODO: Replace the absolute url by a variable. *)
    let css_custom_path = "http://localhost:8080/css/custom.css" in
    let bootstrap_url = "https://cdn.jsdelivr.net/npm\
      /bootstrap@5.2.1/dist/css/bootstrap.min.css"
    in 
    let bootstrap_hash ="sha384-iYQeCzEYFbKjA/\
      T2uDLTpkwGzCiq6soy8tYaI1GyVh/UjpbCx/TYkiZhlZB6+fzT"
    in
    html (head (title (txt str)) [
        meta ~a:[a_charset "utf-8"] ()
      ; meta ~a:[
          a_name "viewport"
        ; a_content "with=device-width, init-scale=1"
      ] () 
      ; link ~a:[a_integrity bootstrap_hash; a_crossorigin `Anonymous]
        ~rel:[`Stylesheet] ~href:bootstrap_url ()
      ; link ~rel:[`Stylesheet] ~href:css_custom_path ()
    ]) (body [
          navbar hcontent
        ; main content
        ; footer fcontent
      ]) 

  let vertical_rule = 
    let open Tyxml in
    [%html {eos|<div class="vr"></div>|eos}]

  let render_404_not_found request =
    page_layout ~subtitle:"404 not found" [Tyxml.Html.txt "404 not found"] 
    |> html_to_string

  let check_selector ~number = 
    let open Tyxml in
    let num = string_of_int number in
    let id = "item_" ^ num in
    [%html "
      <span>" [Html.txt num] "</span>\
      <input class='form-check-input' type='checkbox' form='action-form' \
        id='"id"'/>
    "]

  let selector ~id ~label ?default_option ?placeholder options = 
    let open Tyxml in
    let options = List.map (fun (key, value) -> 
      Html.(option ~a:[a_value value] (txt key))
    ) options in
    let options = 
      match default_option with
      | Some (key, value) -> 
          Html.(option ~a:[a_value value; a_selected ()] (txt key))
            :: options 
      | None -> options
    in
    let options = 
      match placeholder with
      | Some placeholder -> 
          Html.(option ~a:[a_hidden (); a_selected ()] 
            (txt placeholder)) :: options 
      | None -> options
    in
    [%html "
      <div class='input-group'>\
        <label class='input-group-text' for='"id"'>\
          " [Html.txt label] "        
        </label>\
        <select class='form-control mr-sm-2' id='"id"'>\
          " options "\
        </select>\
      </div>\
    "]

  let action_form ~actions =
    let open Tyxml in 
    [%html " 
    <form class='row row-cols-lg-auto g-3 align-items-center'\ 
      name='action-form'>\
      <div class='col-12'>\
        " [selector ~id:"action_kind" ~label:"Action" 
            ~placeholder:"..." actions] "
      </div>\
      <div class='col-12'>\
        <button class='btn btn-outline-success' type='button'>\
          Do\
        </button>\
      </div>\
    </form>\
    "]

  let round_status (round : Round.t) = 
    let open Tyxml.Html in
    match Round.status round with
    | Pending  -> txt "Pending"
    | Running {info; _} | Done {info; _} -> 
        let link = "show/" ^ info.uuid in 
        a ~a:[a_href link] [txt "Done"]
    | Cancelled -> txt "Cancelled"

  let round_provers (round : Round.t) = 
    let open Tyxml.Html in
    match Round.status round with
    | Pending | Cancelled -> txt ""
    | Running {info; _} | Done {info; _} -> 
      let pp fmt el = Format.fprintf fmt "%s" el in
      let provers = List.map fst info.provers |> sprintf_list pp in
      txt provers 

  let round_uuid (round : Round.t) = 
    let open Tyxml.Html in
    match Round.status round with
    | Pending | Cancelled -> txt ""
    | Running {info; _} | Done {info; _} -> 
        txt info.uuid
  
  let round_result (round : Round.t) =
    let open Tyxml.Html in
    match Round.status round with
    | Pending -> txt "Not yet"
    | Running {info; _} | Done {info; _} -> 
        let str = Format.sprintf "%i/%i" info.ctr_suc_pbs info.ctr_pbs in
        txt str 
    | Cancelled -> txt "Cancelled"

  let round_row ~number (round : Round.t) =
    let open Tyxml.Html in
    let date = Round.date round |> format_date in
    tr [
        th (check_selector ~number) 
      ; td [round_provers round] 
      ; td [round_uuid round] 
      ; td ~a:[a_class ["text-center"]] [txt @@ Round.config round] 
      ; td ~a:[a_class ["text-center"]] [txt date]
      ; td ~a:[a_class ["text-center"]] [round_result round]
      ; td ~a:[a_class ["text-center"]] [round_status round] 
    ] 

  let rounds_table rounds =
    let open Tyxml.Html in
    let rows = List.mapi (fun i round ->
      round_row ~number:i round ) rounds 
    in
    tablex ~a:[a_class ["table table-striped table-hover align-middle"]] 
      ~thead:(thead [
        tr [
          th [txt "Select"]
        ; th [txt "Prover"]
        ; th [txt "Uuid"]
        ; th ~a:[a_class ["text-center"]] [txt "Config"]
        ; th ~a:[a_class ["text-center"]] [txt "Date"]
        ; th ~a:[a_class ["text-center"]] [txt "Result"]
        ; th ~a:[a_class ["text-center"]] [txt "Status"]
      ]
      ]) [tbody ~a:[a_class ["table-group-divider"]] rows] 

  let csrf_tag request =
    let open Tyxml in
    let token = Dream.csrf_token request in
    [%html "<input name='dream.csrf' type='hidden' value='" token "'/>"]

  let benchpress_form request =
    let open Tyxml in
    [%html " 
    <form class='row row-cols-lg-auto g-2 align-items-center' \
      method='post' name='benchpress-controller' action='/schedule'>\
      " [csrf_tag request] "
      <div class='col-12'>\
        " [selector ~id:"prover" ~label:"Prover" 
            ~default_option:("default", "default") []] "
      </div>\
      <div class='col-12'>\
        " [selector ~id:"config" ~label:"Config" 
            ~default_option:("default", "default") []] "
      </div>\
      <div class='col-12'>\
        <button class='btn btn-outline-success' type='submit'>\
          Schedule\
        </button>\
      </div>\
    </form>\
    "]

  let rounds_action_form request =
    action_form ~actions:[("compare", "Compare")]
    
  let render_rounds_list rounds request =
    let rounds_table = rounds_table rounds in
    page_layout ~subtitle:"Rounds" ~hcontent:
      [benchpress_form request; rounds_action_form request] [rounds_table] 
    |> html_to_string
 
  let problem_row ~number pb =
    let open Tyxml.Html in
    let open Problem in
    tr [
        th (check_selector ~number) 
      ; td [txt @@ pb.pb_name]
      ; td ~a:[a_class ["text-center"]] 
          [txt @@ string_of_ext pb.pb_ext]
      ; td ~a:[a_class ["text-center"]] 
          [txt @@ string_of_int pb.timeout]
      ; td ~a:[a_class ["text-center"]] 
          [txt @@ string_of_int @@ int_of_error_code pb.error_code]
      ; td ~a:[a_class ["text-center"]] 
          [txt @@ string_of_float pb.rtime]
      ; td ~a:[a_class ["text-center"]] 
          [txt @@ string_of_result pb.res]
      ; td ~a:[a_class ["text-center"]] 
          [txt @@ string_of_result pb.expected_res]
    ]

  let problems_table pbs =
    let open Tyxml.Html in
    let rows =  List.mapi (fun i pb ->
      problem_row ~number:i pb
    ) pbs in
    tablex ~a:[a_class ["table table-striped table-hover align-middle"]] 
      ~thead:(thead [
        tr [
          th [txt "Select"]
        ; th ~a:[a_class ["text-left"]] [txt "Problem"]
        ; th ~a:[a_class ["text-center"]] [txt "Extension"]
        ; th ~a:[a_class ["text-center"]] [txt "Timeout"]
        ; th ~a:[a_class ["text-center"]] [txt "Error code"]
        ; th ~a:[a_class ["text-center"]] [txt "Running time"]
        ; th ~a:[a_class ["text-center"]] [txt "Result"]
        ; th ~a:[a_class ["text-center"]] [txt "Expected"]
      ]
      ]) [tbody ~a:[a_class ["table-group-divider"]] rows] 

  let round_action_form =
    action_form ~actions:[("snapshot", "Snapshot")]

  let filter_form = 
    let open Tyxml in
    [%html {eos|
    <form class="row row-cols-lg-auto g-3 align-items-center" method="get">
      <div class="col-12">
          <div class="input-group">
            <label class="input-group-text">Problem</label>
            <input type="text" class="form-control" placeholder="..."/>
          </div>
      </div>
      <div class="col-12">
        <div class="input-group">
          <label class="input-group-text" for="error_code">Error code</label>
          <select class="form-control" id="error_code">
            <option value="any" selected>any</option>
            <option value="0">0</option>
            <option value="1">1</option>
            <option value="123">123</option>
          </select>
        </div>
      </div>
      <div class="col-12">
        <div class="input-group">
          <label class="input-group-text" for="res">Result</label>
          <select class="form-control" id="res">
            <option value="any" selected>any</option>
            <option value="unsat">unsat</option>
            <option value="sat">sat</option>
            <option value="unknown">unknown</option>
            <option value="error">error</option>
          </select>
        </div>
      </div>
      <div class="col-12">
        <div class="input-group">
          <label class="input-group-text" for="expected_res">Expected</label>
          <select class="form-control" id="expected_res">
            <option value="any" selected>any</option>
            <option value="unsat">unsat</option>
            <option value="sat">sat</option>
            <option value="unknown">unknown</option>
            <option value="error">error</option>
          </select>
        </div>
      </div>
      <div class="col-12">
        <button class="btn btn-outline-success" type="submit">
          Filter
        </button>
      </div>
    </form>|eos} ]
 
  let render_round_detail pbs request =
    let table = problems_table pbs in
    page_layout ~subtitle:"Round" ~hcontent:
      [filter_form; vertical_rule; round_action_form] [table]
    |> html_to_string 

  let render_problem_detail pb request = 
    failwith "Not implemented yet."
end

module Rounds_queue : sig
  type t 

  val make: dir:string -> t Lwt.t
end = struct
  type t = {
    pending_rounds: Round.t Queue.t;
    running_round: Round.t option;
    done_rounds: Round.t list
  }

  let make ~dir = 
    let%lwt pending_rounds = File.readdir 
      ~ext_filter:(fun str -> String.equal str ".sqlite") dir 
      |> Lwt_list.map_s (fun db_file -> 
          match%lwt Round.resurect ~db_file with
          | Ok round -> Lwt.return round
          | Error _ -> failwith "Cannot restore previous rounds.") 
    in
    let pending_rounds = List.to_seq pending_rounds |> Queue.of_seq in
    Lwt.return { pending_rounds; running_round = None; done_rounds = [] } 
end

let resurected_rounds () = 
  let%lwt rounds = File.readdir 
      ~ext_filter:(fun str -> String.equal str ".sqlite") 
      benchpress_share_dir 
    |> Lwt_list.map_s (fun db_file -> 
        match%lwt Round.resurect ~db_file with
        | Ok round -> Lwt.return round
        | Error _ -> failwith "Cannot restore previous rounds.")
  in
  Lwt.return @@ ref (List.sort Round.compare rounds)

let ugly res tl = 
  match%lwt res with
  | Ok res -> Lwt.return (res :: tl)
  | Error str ->
      Dream.log "%s" str;
      Lwt.return tl

module Handlers : sig
  type ctx = Round.t list ref Lwt.t
  
  val handle_rounds_list : ctx -> Dream.handler
  val handle_round_detail : ctx -> Dream.handler
  val handle_schedule_round : ctx -> Dream.handler
  val handle_stop_round : ctx -> Dream.handler
end = struct 
  type ctx = Round.t list ref Lwt.t
  
  let handle_rounds_list ctx request = 
    let%lwt rounds = ctx in
    let%lwt r = match !rounds with
    | [] -> Lwt.return !rounds
    | hd :: tl -> begin 
        match Round.status hd with
        | Pending -> ugly (Round.run hd) tl 
        | _ -> Lwt.return (hd :: tl) 
    end in
    rounds := r;
    Views.render_rounds_list !rounds request
    |> Dream.html

  let handle_round_detail ctx request = 
    let uuid = Dream.param request "uuid" in
    let%lwt rounds = ctx in
    match List.find_opt (fun (round : Round.t) ->
      match Round.status round with 
      | Pending | Running _ | Cancelled -> false
      | Done {info; _} -> String.equal info.uuid uuid
    ) !rounds with
    | Some round -> begin
        match%lwt Round.problems round  with
        | Ok pbs ->
            Views.render_round_detail pbs request |> Dream.html 
        | Error _ -> 
            Views.render_404_not_found request |> Dream.html
        end
    | None -> 
        Views.render_404_not_found request |> Dream.html

  let handle_schedule_round ctx request =
    let%lwt rounds = ctx in 
    rounds := Round.make () :: !rounds;
    match%lwt Dream.form request with 
    | `Ok _ -> Dream.redirect request "/"
    | _ -> Dream.empty `Bad_Request

  let handle_stop_round ctx request = 
    match%lwt Dream.form request with 
    | `Ok _ -> Dream.redirect request "/"
    | _ -> Dream.empty `Bad_Request
end

let () =
  let ctx = resurected_rounds () in
  Dream.run 
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [
      Dream.get "/css/**" @@ Dream.static (List.hd Location.Sites.css)
    ; Dream.get "/" @@ Handlers.handle_rounds_list ctx
    ; Dream.get "/show/:uuid" @@ Handlers.handle_round_detail ctx 
    ; Dream.post "/schedule" @@ Handlers.handle_schedule_round ctx 
    ; Dream.get "/stop/:uuid" @@ Handlers.handle_stop_round ctx 
  ] 
