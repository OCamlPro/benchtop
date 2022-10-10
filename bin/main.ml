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
  type 'a request 
  type 'a answer = ('a, Caqti_error.t) Lwt_result.t

  val exec : db_file:string -> 'a request -> 'a answer
  val (@) : 'a request -> 'b request -> ('a * 'b) request
  val debug : 'a answer -> ('a, string) Lwt_result.t

  val round_info : Round_info.t request
  val select_problems : Problem.t list request
  val provers : (string * string) list request
  val set_wal : int request
end = struct
  type 'a answer = ('a, Caqti_error.t) Lwt_result.t
  type 'a request = (module Caqti_lwt.CONNECTION) -> 'a answer

  let exec ~db_file req = 
    let db_uri = 
      "sqlite3://" ^ Filename.concat benchpress_share_dir db_file 
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

    let set_wal = 
      unit ->! int @@
      "PRAGMA busy_timeout = 5000"
  end

  let round_info (module Db : Caqti_lwt.CONNECTION) =
    Lwt_result.bind (Db.find Query_strings.meta_info ())
      (fun (uuid, timestamp, ctr_pbs, ctr_suc_pbs) ->
        Lwt_result.bind_result (Db.collect_list Query_strings.provers ())
        (fun provers ->
          let round_info = Round_info.make 
            ~provers ~uuid ~timestamp ~ctr_pbs ~ctr_suc_pbs
          in 
          Ok round_info))
  
  let select_problems (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Query_strings.select_problems ()

  let provers (module Db : Caqti_lwt.CONNECTION) = 
    Db.collect_list Query_strings.provers () 

  let set_wal (module Db : Caqti_lwt.CONNECTION) =
    Db.find Query_strings.set_wal ()
end

type process_error = [
  | `Stopped of Unix.process_status
  | `Db_not_found of Unix.process_status 
]

type round_error = [
  | `Cannot_retrieve_info of string
  | `Not_done
]

type error = [ Caqti_error.t | process_error | round_error ]

module Process : sig 
  type t
  
  val run : cmd:Lwt_process.command -> config:string -> 
    (t, error) Lwt_result.t
  val stop : t -> Unix.process_status Lwt.t
  val is_done : t -> bool 
  val db_file : t -> string
end = struct
  type t = {
    inotify: Lwt_inotify.t;
    handler: Lwt_process.process_none;
    stdout: in_channel;
    stderr: in_channel;
    db_file: string;
  }

  let rec wait_db_file =
    let is_db file = 
      String.equal (Filename.extension file) ".sqlite"
    in
    fun inotify ->
    match%lwt Lwt_inotify.read inotify with
      | (_, [Inotify.Create], _, Some file) when is_db file -> 
          Lwt_result.return file
      | _ -> wait_db_file inotify

  let wait_terminate handler = 
    let%lwt rc = handler#status in
    Lwt_result.fail rc

  let create_temp_file suffix =
    let file = Filename.temp_file "benchpress" suffix in
    let fd = Unix.openfile file [O_RDWR] 0o640 in
    (fd, Unix.in_channel_of_descr fd)

  let run ~cmd ~config =
    let%lwt inotify = Lwt_inotify.create () in
    let%lwt _ = 
      Lwt_inotify.add_watch inotify benchpress_share_dir 
        [Inotify.S_Create] 
    in
    let stdout_fd, stdout = create_temp_file "stdout" in 
    let stderr_fd, stderr = create_temp_file "stderr" in 
    Dream.info (fun log -> log "Ready to run benchpress");
    let handler = Lwt_process.open_process_none ~stdout:(`FD_copy stdout_fd) 
      ~stderr:(`FD_copy stderr_fd) cmd in
    Dream.info (fun log -> log "Running benchpress");
    match%lwt Lwt.choose [wait_db_file inotify; 
        wait_terminate handler] with
      | Ok db_file -> 
          Dream.info (fun log -> log "Found the database %s" db_file);
          Lwt_result.return {inotify; stdout; stderr; handler; db_file}
      | Error rc ->
          Dream.error (fun log -> log "Cannot found the database");
          Dream.log "%s" (File.read_all stdout);
          Lwt_result.fail (`Db_not_found rc) 

  let stop {handler; _} = handler#status
  
  let is_done {handler; _} =
    match handler#state with
    | Running -> false
    | Exited _ -> true 

  let db_file {db_file; _} = db_file
end

module Round : sig 
  type status = 
    | Pending of (Process.t, error) Lwt_result.t Lazy.t 
    | Running of Process.t 
    | Failed of error
    | Done of {db_file: string; info: Round_info.t}
  
  type t = private {
    config: string;
    date: Unix.tm;
    status: status
  }

  val make : cmd:Lwt_process.command -> config:string -> t
  val resurect : db_file:string -> t Lwt.t
  val run : t -> t Lwt.t 
  val update : t -> t Lwt.t
  val stop : t -> t Lwt.t
  val is_done : t -> bool

  val problems : t -> (Problem.t list, string) Lwt_result.t
end = struct
  type status = 
    | Pending of (Process.t, error) Lwt_result.t Lazy.t 
    | Running of Process.t 
    | Failed of error
    | Done of {db_file: string; info: Round_info.t}
  
  type t = {
    config: string;
    date: Unix.tm;
    status: status
  }

  let now () = Unix.time () |> Unix.localtime

  let make ~cmd ~config = 
    let status = Pending (lazy (Process.run ~cmd ~config)) in
    {config; date = now (); status} 

  let retrieve_info db_file =
    let request = 
      Request.exec ~db_file Request.round_info
      |> Request.debug
    in
    let%lwt date, status = match%lwt request with 
    | Ok info -> Lwt.return (info.running_at, Done {db_file; info})
    | Error err -> Lwt.return (now (), Failed (`Cannot_retrieve_info err))
    in
    Lwt.return {config = "default"; date; status}

  let resurect ~db_file = retrieve_info db_file

  let run round = 
    match round.status with
    | Pending susp -> begin
        let%lwt status = match%lwt Lazy.force susp with
        | Ok proc -> Lwt.return (Running proc)
        | Error err -> Lwt.return (Failed err)
        in
        Lwt.return {round with date = now(); status}
      end
    | Running _ | Failed _ | Done _ -> Lwt.return round 

  let update round =
    match round.status with
    | Running proc ->
        if Process.is_done proc then
          Process.db_file proc |> retrieve_info 
        else Lwt.return round
    | Pending _ | Failed _ | Done _ -> Lwt.return round

  let stop round =
    match round.status with
    | Running proc when not @@ Process.is_done proc -> 
        let%lwt rc = Process.stop proc in
        Lwt.return {round with date = now (); status = Failed (`Stopped rc)}
    | Pending _ | Running _ | Failed _ | Done _ -> Lwt.return round 

  let is_done round =
    match round.status with
    | Pending _ -> false
    | Running proc -> Process.is_done proc
    | Failed _ | Done _ -> true

  let problems round =
    match round.status with
    | Done {db_file; _} -> 
        Request.exec ~db_file Request.select_problems 
        |> Request.debug
    | Pending _ | Running _ | Failed _ -> 
        Lwt_result.fail "Not available"
end

module Rounds_queue : sig 
  type t

  val make : dir:string -> t Lwt.t
  val update : t -> t Lwt.t
  val push : Round.t -> t -> t
  val to_list : t -> Round.t list
  val find_by_uuid : string -> t -> Round.t option
end = struct
  type t = {
    lst: Round.t list;
    pos: int option
  }

  let to_list {lst; _} = lst

  let make ~dir =     
    let ext_filter = fun str -> String.equal str ".sqlite" in
    let%lwt lst = 
      File.readdir ~ext_filter dir 
      |> Lwt_list.map_s (fun db_file -> Round.resurect ~db_file) 
    in
    Lwt.return {lst; pos = None}

  let update {lst; pos} =
    let new_pos = ref pos in
    let%lwt lst = Lwt_list.mapi_s (fun j (round : Round.t) ->  
      match (pos, round.status) with
      | Some i, Pending _ when i = j ->
          Round.run round
      | Some i, Done _ | Some i, Failed _ when i = j ->
          new_pos := if i > 0 then Some (i-1) else None;
          Round.update round
      | _ -> Round.update round) lst in
    Lwt.return {lst; pos = !new_pos}

  let push round {lst; pos} = 
    let pos = 
      match pos with 
      | Some i -> Some (i+1)
      | None -> Some 0
    in
    {lst = round :: lst; pos}

  let find_by_uuid uuid {lst; _} = 
    List.find_opt (fun (round : Round.t) -> 
      match round.status with
      | Done {info; _} -> String.equal uuid info.uuid
      | Pending _ | Running _ | Failed _ -> false) lst
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

  let csrf_tag request =
    let open Tyxml in
    let token = Dream.csrf_token request in
    [%html "<input name='dream.csrf' type='hidden' value='" token "'/>"]

  let navbar content = 
    let open Tyxml in
    [%html "
      <header class='navbar bg-light sticky-top'>\
        <div class='container-fluid'>\
          <a class='navbar-brand text-primary' href='#'>Benchtop</a>"
          content  
        "</div>\
      </header>
    "]

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

  module Selector = struct 
    type default_option = 
      | Default_value of {key: string; value: string}
      | Placeholder of string
      | None

    let make ~id ~label ?(default_option=None) ?current options = 
      let open Tyxml in
      let options = List.map (fun (key1, value) -> 
        let attributs = match current with
        | Some key2 when String.equal key1 key2 -> 
            Html.[a_value value; a_selected ()]
        | Some _ | None -> 
            Html.[a_value value]
        in
        Html.(option ~a:attributs (txt key1))
      ) options in
      let options =
        let selected_default = match current with
        | Some _ -> []
        | None -> [Html.a_selected ()] 
        in
        match default_option with
        | Default_value {key; value} -> 
            Html.(option ~a:(a_value value :: selected_default) (txt key))
              :: options 
        | Placeholder key -> 
            Html.(option ~a:(a_hidden () :: selected_default) 
              (txt key)) :: options 
        | None -> options
      in
      [%html "
        <div class='input-group'>\
          <label class='input-group-text' for='"id"'>\
            " [Html.txt label] "\        
          </label>\
          <select class='form-control mr-sm-2' id='"id"' name='"id"'>\
            " options "\
          </select>\
        </div>\
      "]
  end

  let action_form request ~actions =
    let open Tyxml in 
    [%html " 
      <form class='row row-cols-lg-auto g-3 align-items-center'\ 
        name='action-form' method='post'>\
        " [csrf_tag request] "
        <div class='col-12'>\
          " [Selector.make ~id:"action_kind" ~label:"Action" 
              ~default_option:(Placeholder "...") actions] "
        </div>\
        <div class='col-12'>\
          <button class='btn btn-outline-success' type='submit'>\
            Do\
          </button>\
        </div>\
      </form>\
    "]

  let round_status (round : Round.t) = 
    let open Tyxml.Html in
    match round.status with
    | Pending _  -> txt "Pending"
    | Running _  -> txt "Running"
    | Done {info; _} -> 
        let link = "show/" ^ info.uuid in 
        a ~a:[a_href link] [txt "Done"]
    | Failed _ -> txt "Error" 

  let round_provers (round : Round.t) = 
    let open Tyxml.Html in
    match round.status with
    | Pending _ | Running _ | Failed _ -> txt ""
    | Done {info; _} -> 
      let pp fmt el = Format.fprintf fmt "%s" el in
      let provers = List.map fst info.provers |> sprintf_list pp in
      txt provers 

  let round_uuid (round : Round.t) = 
    let open Tyxml.Html in
    match round.status with
    | Pending _ | Running _ | Failed _ -> txt ""
    | Done {info; _} -> 
        txt info.uuid
  
  let round_result (round : Round.t) =
    let open Tyxml.Html in
    match round.status with
    | Pending _ | Running _ -> txt "Not yet"
    | Done {info; _} -> 
        let str = Format.sprintf "%i/%i" info.ctr_suc_pbs info.ctr_pbs in
        txt str 
    | Failed _ -> txt "Error"

  let round_row ~number (round : Round.t) =
    let open Tyxml.Html in
    let date = round.date |> format_date in
    tr [
        th (check_selector ~number) 
      ; td [round_provers round] 
      ; td [round_uuid round] 
      ; td ~a:[a_class ["text-center"]] [txt @@ round.config] 
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

  let benchpress_form request =
    let open Tyxml in
    [%html " 
    <form class='row row-cols-lg-auto g-2 align-items-center' \
      method='post' name='benchpress-controller' action='/schedule'>\
      " [csrf_tag request] "
      <div class='col-12'>\
        " [Selector.make ~id:"prover" ~label:"Prover" 
            ~default_option:(Default_value {key="default"; value="default"}) 
            []] "
      </div>\
      <div class='col-12'>\
        " [Selector.make ~id:"config" ~label:"Config" 
            ~default_option:(Default_value {key="default"; value="default"})
            []] "
      </div>\
      <div class='col-12'>\
        <button class='btn btn-outline-success' type='submit'>\
          Schedule\
        </button>\
      </div>\
    </form>\
    "]

  let rounds_action_form request =
    action_form request ~actions:[("compare", "Compare")]
    
  let render_rounds_list rounds request =
    let rounds_table = rounds_table rounds in
    page_layout ~subtitle:"Rounds" ~hcontent:
      [benchpress_form request; rounds_action_form request] [rounds_table] 
    |> html_to_string

  let color_of_result = function
    | Sat -> ["text-success"]
    | Unsat -> ["text-primary"]
    | Unknown -> ["text-orange"]
    | Error -> ["text-danger"]
 
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
      ; td ~a:[a_class (["text-center"] @ color_of_result pb.res)] 
          [txt @@ string_of_result pb.res]
      ; td ~a:[a_class (["text-center"] @ color_of_result pb.expected_res)] 
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

  let filter_form request = 
    let open Tyxml in
    [%html "
    <form class='row row-cols-lg-auto g-3 align-items-center' method='get' \
      >\
      <div class='form-check form-switch'>\
        <input class='form-check-input' type='checkbox' role='switch' 
          id='switch-diff'>\
        <label class='form-check-label' for='switch-diff'>\
          Diff\
        </label>\
      </div>
      <div class='col-12'>\
          <div class='input-group'>\
            <label class='input-group-text'>Problem</label>\
            <input type='text' class='form-control' name='problem' \
              placeholder='...'/>\
          </div>\
      </div>\
      <div class='col-12'>\
      " [Selector.make ~id:"error_code" ~label:"Error code" 
          ~default_option:(Default_value {key="any"; value="any"}) [
          ("0", "0"); ("<> 0", "<> 0"); ("1", "1"); ("123", "123")
        ]] "
     </div>\
     <div class='col-12'>\
      " [Selector.make ~id:"res" ~label:"Result"
          ~default_option:(Default_value {key="any"; value="any"}) [
          ("unsat", "unsat"); ("sat", "sat"); ("unknown", "unknown"); 
          ("error", "error")
        ]] "
     </div>\ 
     <div class='col-12'>\
      " [Selector.make ~id:"expected_res" ~label:"Expected"
          ~default_option:(Default_value {key="any"; value="any"}) [
          ("unsat", "unsat"); ("sat", "sat"); ("unknown", "unknown"); 
          ("error", "error")
        ]] "
     </div>\
     <div class='col-12'>\
        <button class='btn btn-outline-success' type='submit'>\
          Filter\
        </button>\
      </div>\
    </form>\
    "]
 
  let render_round_detail pbs request =
    let table = problems_table pbs in
    page_layout ~subtitle:"Round" ~hcontent:
      [filter_form request; vertical_rule; round_action_form request] [table]
    |> html_to_string 

  let render_problem_detail pb request = 
    failwith "Not implemented yet."
end

type ctx = { 
  mutable queue: Rounds_queue.t
}

module Handlers : sig
  val handle_rounds_list : ctx Lwt.t -> Dream.handler
  val handle_round_detail : ctx Lwt.t -> Dream.handler
  (*val handle_problem_trace : Dream.handler*)
  val handle_schedule_round : ctx Lwt.t -> Dream.handler
  val handle_stop_round : ctx Lwt.t -> Dream.handler
end = struct 
  let handle_rounds_list ctx request =
    let%lwt ({queue; _} as ctx) = ctx in 
    let%lwt queue = Rounds_queue.update queue in
    ctx.queue <- queue;
    Views.render_rounds_list (Rounds_queue.to_list queue) request
    |> Dream.html

  let handle_round_detail ctx request = 
    let uuid = Dream.param request "uuid" in
    let%lwt {queue; _} = ctx in 
    match Rounds_queue.find_by_uuid uuid queue with
    | Some round -> begin
        match%lwt Round.problems round with
        | Ok pbs ->
            Views.render_round_detail pbs request |> Dream.html 
        | Error _ -> 
            Views.render_404_not_found request |> Dream.html
        end
    | None -> 
        Views.render_404_not_found request |> Dream.html

  (*let handle_problem_trace ctx request =
    let uuid = Dream.param request "uuid" in
    let problem_name = Dream.param request "problem" in
    match%lwt Round.problem round problem name with
    | Ok pb ->
        Views.render_problem_trace pb request |> Dream.html
    | Error _ -> 
        Views.render_404_not_found request |> Dream.html*)

  let handle_schedule_round ctx request =
    let%lwt ({queue; _} as ctx) = ctx in 
    let new_round = Round.make ~cmd:
      ("benchpress", [|"benchpress"; "run"; "-c"; 
        "lib/config.sexp"; "-p"; "alt-ergo"; "lib/tests"|]) ~config:"default" in
    ctx.queue <- Rounds_queue.push new_round queue;
    match%lwt Dream.form request with 
    | `Ok _ -> Dream.redirect request "/"
    | _ -> Dream.empty `Bad_Request

  let handle_stop_round ctx request = 
    match%lwt Dream.form request with 
    | `Ok _ -> Dream.redirect request "/"
    | _ -> Dream.empty `Bad_Request
end

let () = 
  let ctx = 
    let%lwt queue = Rounds_queue.make ~dir:benchpress_share_dir in
    Lwt.return {queue}
  in
  Dream.initialize_log ~level:`Debug ();
  Dream.run 
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [
      Dream.get "/css/**" @@ Dream.static (List.hd Location.Sites.css)
    ; Dream.get "/" @@ Handlers.handle_rounds_list ctx 
    ; Dream.get "/show/:uuid" @@ Handlers.handle_round_detail ctx
    (*; Dream.get "/show/:uuid/problem/:problem" @@ Handlers.handle_problem_trace *)
    ; Dream.post "/schedule" @@ Handlers.handle_schedule_round ctx
    ; Dream.get "/stop" @@ Handlers.handle_stop_round ctx 
  ] 
