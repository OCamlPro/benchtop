open Benchtop

let benchpress_share_dir = "/home/tiky/.local/share/benchpress" 

let string_of_int = Format.sprintf "%i"
let string_of_float = Format.sprintf "%f"

let int_of_error_code = 
  let open Unix in function
  | WEXITED code | WSIGNALED code | WSTOPPED code -> code

let error_code_of_int i =
  Unix.WEXITED i

type res = Sat | Unsat | Unknown | Error
type ext = Ae | Smt2 | Psmt2

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
    date: Unix.tm;
    ctr_pbs : int;
    ctr_suc_pbs: int;
  }

  val make: provers: (string * string) list -> uuid: string -> 
    timestamp: float -> ctr_pbs: int -> ctr_suc_pbs: int -> t
end = struct
  type t = {
    provers: (string * string) list;
    uuid: string;
    date: Unix.tm;
    ctr_pbs : int;
    ctr_suc_pbs: int;
  }

  let make ~provers ~uuid ~timestamp ~ctr_pbs ~ctr_suc_pbs =
    let date = Unix.localtime timestamp in
    {provers; uuid; date; ctr_pbs; ctr_suc_pbs}
end

module Request : sig 
  type 'a request = (module Caqti_lwt.CONNECTION) -> 
    ('a, string) result Lwt.t

  val round_info : Round_info.t request

  val select_problems : Problem.t list request
end = struct
  type 'a request = (module Caqti_lwt.CONNECTION) -> 
    ('a, string) result Lwt.t

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
  end

  let round_info (module Db : Caqti_lwt.CONNECTION) =
    match%lwt Db.find Query_strings.meta_info () with
    | Ok (uuid, timestamp, ctr_pbs, ctr_suc_pbs) -> begin
      match%lwt Db.collect_list Query_strings.provers () with
      | Ok provers ->
          let round_info = Round_info.make 
            ~provers ~uuid ~timestamp ~ctr_pbs ~ctr_suc_pbs
          in 
          Lwt.return (Ok round_info) 
      | Error err -> 
        Dream.log "%a" Caqti_error.pp err;
        Lwt.return (Result.error "Cannot get the information of the round")
    end
    | Error err -> 
        Dream.log "%a" Caqti_error.pp err;
        Lwt.return (Result.error "Cannot get the information of the round")

  let select_problems (module Db : Caqti_lwt.CONNECTION) =
    match%lwt Db.collect_list Query_strings.select_problems () with
    | Ok _ as res -> Lwt.return res
    | Error err -> 
        Dream.log "%a" Caqti_error.pp err;
        Lwt.return (Result.Error "Cannot get the list of problems")
end

module Round : sig
  type process
  
  type status =  
    | Pending     
    | Running of process 
    | Done of Round_info.t
    | Cancelled 

  type t = private {
    cmd: Lwt_process.command;
    config: string;
    date: Unix.tm;
    db_file: string option;
    status: status
  }

  val make : args: string array -> t
  val resurect : db_file: string -> t Lwt.t
  val update : (module Caqti_lwt.CONNECTION) -> t -> t Lwt.t
  val run : t -> t
  val cancel : t -> t

  val problems : t -> Problem.t list Lwt.t 
end = struct 
  type process = Lwt_process.process_none

  type status = 
    | Pending 
    | Running of process 
    | Done of Round_info.t
    | Cancelled 

  type t = {
    cmd: Lwt_process.command;
    config: string;
    date: Unix.tm;
    db_file: string option;
    status: status
  }
  
  let make ~args = {
    cmd = ("benchpress", args); 
    config = "default";
    date = Unix.time () |> Unix.localtime;
    db_file = None;
    status = Pending 
  }

  let connect round = 
    match round.db_file with
    | Some db_file -> begin
      let db_uri = 
        "sqlite3://" ^ Filename.concat benchpress_share_dir db_file 
        |> Uri.of_string
      in
      match%lwt Caqti_lwt.connect db_uri with
      | Ok con -> Lwt.return (Some con)
      | Error err ->
          Dream.log "%a" Caqti_error.pp err; 
          Lwt.return None
      end
    | None -> failwith "Cannot connect the database"

  let update db round = 
    match round.status with
    | Pending | Running _ | Done _ -> begin 
      match%lwt Request.round_info db with
      | Ok info -> 
          Lwt.return { round with date = info.date; status = Done info }
      | Error msg -> 
          Dream.log "%s" msg; 
          Lwt.return round
      end
    | Cancelled -> Lwt.return round

  let resurect ~db_file = 
    let round = { (make ~args:[||]) with db_file = Some db_file } in
    match%lwt connect round with
    | Some con -> update con round 
    | None -> 
        Dream.log "Cannot resurect the round %s" db_file;
        Lwt.return round
  
  let run round = 
    match round.status with
    | Pending ->  
        let proc = Lwt_process.open_process_none round.cmd in 
        let date = Unix.time () |> Unix.localtime in
        { round with status = Running proc; date } 
    | Running _ | Done _ | Cancelled -> round 

  let cancel round =  
    match round.status with
    | Pending -> { round with status = Cancelled }    
    | Running proc -> begin 
        proc#terminate;
        ignore proc#close;
        { round with status = Cancelled } 
      end
    | Done _ | Cancelled -> round

  let problems round =
    match round.status with
    | Done info -> begin
      match%lwt connect round with
      | Some con -> begin
          match%lwt Request.select_problems con with
          | Ok lst -> Lwt.return lst
          | Error _ -> Lwt.return []
        end
      | None ->
          Dream.log "Cannot get problems list";
          Lwt.return []
      end
    | Pending | Running _ | Cancelled -> Lwt.return []
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
    Format.sprintf "%02i/%02i/%04i %i:%02i:%02i"
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
    [%html
      "<span>" [Html.txt num] "</span>\
      <input class=\"form-check-input\" type=\"checkbox\" form=\"action-form\"
        id="id" />"
    ]

  let action_form ~actions =
    let open Tyxml in 
    [%html {eos| 
    <form class="row row-cols-lg-auto g-3 align-items-center" 
      name="action-form">
      <div class="col-12">
        <div class="input-group">
          <select class="form-control" id="action_kind">|eos} 
            actions
          {eos|</select> 
        </div>
      </div>
      <div class="col-12">
        <button class="btn btn-outline-success" type="button">
          Do
        </button>
      </div>
    </form>|eos} ]

  let round_status (round : Round.t) = 
    let open Tyxml.Html in
    match round.status with
    | Pending  -> txt "Pending"
    | Running _ -> txt "Running"
    | Done info -> 
        let link = "show/" ^ info.uuid in 
        a ~a:[a_href link] [txt "Done"]
    | Cancelled -> txt "Cancelled"

  let round_provers (round : Round.t) = 
    let open Tyxml.Html in
    match round.status with
    | Pending | Running _ | Cancelled -> txt ""
    | Done info -> 
      let pp fmt el = Format.fprintf fmt "%s" el in
      let provers = List.map fst info.provers |> sprintf_list pp in
      txt provers 

  let round_uuid (round : Round.t) = 
    let open Tyxml.Html in
    match round.status with
    | Pending | Running _ | Cancelled -> txt ""
    | Done info -> 
        txt info.uuid
  
  let round_result (round : Round.t) =
    let open Tyxml.Html in
    match round.status with
    | Pending | Running _ -> txt "Not yet"
    | Done info -> 
        let str = Format.sprintf "%i/%i" info.ctr_suc_pbs info.ctr_pbs in
        txt str 
    | Cancelled -> txt "Cancelled"

  let round_row ~number (round : Round.t) =
    let open Tyxml.Html in
    let date = round.date |> format_date in
    tr [
        th (check_selector ~number) 
      ; td [round_provers round] 
      ; td ~a:[a_class ["text-center"]] [round_uuid round] 
      ; td ~a:[a_class ["text-center"]] [txt round.config] 
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
        ; th ~a:[a_class ["text-center"]] [txt "Uuid"]
        ; th ~a:[a_class ["text-center"]] [txt "Config"]
        ; th ~a:[a_class ["text-center"]] [txt "Date"]
        ; th ~a:[a_class ["text-center"]] [txt "Result"]
        ; th ~a:[a_class ["text-center"]] [txt "Status"]
      ]
      ]) [tbody ~a:[a_class ["table-group-divider"]] rows] 

  let benchpress_form =
    let open Tyxml in
    [%html {eos| 
    <form class="row row-cols-lg-auto g-2 align-items-center" 
      method="post" name="benchpress-controller">
      <div class="col-12">
        <div class="input-group">
          <label class="input-group-text" for="config">Prover</label>
          <select class="form-control mr-sm-2" id="config"> 
            <option value="default">default</option>
          </select> 
        </div>
      </div>
      <div class="col-12">
        <div class="input-group">
          <label class="input-group-text" for="config">Config</label>
          <select class="form-control mr-sm-2" id="config"> 
            <option value="default">default</option>
          </select> 
        </div>
      </div>
      <div class="col-12">
        <button class="btn btn-outline-success" type="button">
          Schedule
        </button>
      </div>
    </form>|eos} ]

  let rounds_action_form =
    let open Tyxml in 
    let%html actions = "
      <option selected hidden>action</option>\
      <option value='compare'>compare</option> "
    in
    action_form ~actions
    
  let render_rounds_list rounds request =
    let rounds_table = rounds_table rounds in
    page_layout ~subtitle:"Rounds" ~hcontent:
      [benchpress_form; rounds_action_form] [rounds_table] 
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
    let open Tyxml in 
    let%html actions = "
      <option selected hidden>action</option>\
      <option value='compare'>snapshot</option> "
    in
    action_form ~actions

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
        <button class="btn btn-outline-success" type="button">
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

module Handlers : sig
  val handle_rounds_list : Round.t list Lwt.t ref -> Dream.handler
  val handle_round_detail : Round.t list Lwt.t ref -> Dream.handler
end = struct 
  let handle_rounds_list rounds request = 
    let%lwt rounds = !rounds in 
    let html = Views.render_rounds_list rounds request in
    Dream.html html

  let handle_round_detail rounds request = 
    let uuid = Dream.param request "uuid" in
    let%lwt rounds = !rounds in 
    match List.find_opt (fun (round : Round.t) ->
      match round.status with 
      | Pending | Running _ | Cancelled -> false
      | Done info -> String.equal info.uuid uuid
    ) rounds with
    | Some round ->
        let%lwt pbs = Round.problems round in
        Views.render_round_detail pbs request |> Dream.html 
    | None -> 
        Views.render_404_not_found request |> Dream.html
end

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
end

let () =
  let resurected_rounds : Round.t list Lwt.t =  
    File.readdir 
      ~ext_filter:(fun str -> String.equal str ".sqlite") 
      benchpress_share_dir 
    |> Lwt_list.map_s (fun db_file -> Round.resurect ~db_file) 
  in
  let rounds = ref resurected_rounds in
  Dream.run 
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [
      Dream.get "/css/**" @@ Dream.static (List.hd Location.Sites.css)
    ; Dream.get "/" @@ Handlers.handle_rounds_list rounds
    ; Dream.get "/show/:uuid" @@ Handlers.handle_round_detail rounds
  ] 
