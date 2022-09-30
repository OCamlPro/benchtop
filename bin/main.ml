open Benchtop

let benchpress_share_dir = "/home/tiky/.local/share/benchpress" 

let rev_mapi fn = 
  let rec aux i acc = function
  | [] -> acc
  | hd :: tl -> 
      aux (i+1) (fn i hd :: acc) tl 
  in
  aux 0 []

let string_of_int = Format.sprintf "%i"
let string_of_float = Format.sprintf "%f"


let int_of_error_code = 
  let open Unix in function
  | WEXITED code | WSIGNALED code | WSTOPPED code -> code

let error_code_of_int i =
  Unix.WEXITED i

module Problem : sig 
  type t
  type res = Sat | Unsat | Unknown | Error
  
  val is_successful : t -> bool
end = struct
  type res = Sat | Unsat | Unknown | Error
  
  type t = {
    prover: string;
    file_path: string;
    res: res;
    expected_res: res;
    timeout: int;
    error_code: Unix.process_status;
    rtime: float 
  }
  let string_of_result = function
    | Sat -> "sat"
    | Unsat -> "unsat"
    | Unknown -> "unknown"
    | Error -> "error"

  let result_of_string = function
    | "sat" -> Sat
    | "unsat" -> Unsat
    | "unknown" -> Unknown
    | "error" -> Error
    | str -> 
      failwith (Format.sprintf "Unexpected result: %s" str)

  let make ~prover ~file_path ~res ~file_expect ~timeout ~errcode ~rtime =
    let res = result_of_string res in
    let expected_res = result_of_string file_expect in
    let error_code = error_code_of_int errcode in
    {prover; file_path; res; expected_res; timeout; error_code; rtime} 

  let is_successful pb =
    match (pb.res, pb.expected_res) with
    | Sat, Sat | Unsat, Unsat 
    | Unknown, Unknown | Error, Error -> true
    | _ -> false 
end

module Request : sig 
  type ('a, 'b) t = (module Caqti_lwt.CONNECTION) -> 
    ('a, [> Caqti_error.call_or_retrieve] as 'b) result Lwt.t

  val select_problems : ? -> (Problem.t list, _) t

  val count_tests : (int, _) t
  val count_successful_tests : (int, _) t
end 
= struct
  type ('a, 'b) t = (module Caqti_lwt.CONNECTION) -> 
    ('a, [> Caqti_error.call_or_retrieve] as 'b) result Lwt.t

  module Query_strings = struct
    open Caqti_request.Infix
    open Caqti_type.Std
 
    let problem =
      let open Problem in
      let encode {prover; file_path; result; expected_result; error_code} =
        let res = string_of_result result in
        let file_expect = string_of_result expected_result in
        Ok (prover, (file_path, (res, (file_expect, error_code))))
      in
      let decode 
        (prover, 
          (file, 
            (res, 
              (file_expect, 
                (errcode, rtime))))) = 
        Ok Problem.make prover; file_path; result; 
          expected_result; error_code} 
      in
      let rep_ty = 
        Caqti_type.(tup2 string 
          (tup2 string 
            (tup2 string 
              (tup2 string (tup2 int, float)))))  
      in
      custom ~encode ~decode rep_ty

    let select_problems ?(successful = false) () =
      let test = if successful then
        "WHERE res = file_expect"
      else "" in
      unit ->* problem @@ 
      Format.sprintf {eos|
        SELECT 
          prover, file, res, 
          file_expect, errcode 
        FROM prover_res 
        %s
      |eos} test

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

  let select_problems (module Db : Caqti_lwt.CONNECTION) =
    Db.collect_list Query_strings.select_problems ()

  let count_tests (module Db : Caqti_lwt.CONNECTION) =
    Db.find Query_strings.count_tests ()

  let count_successful_tests (module Db : Caqti_lwt.CONNECTION) =
    Db.find Query_strings.count_successful_tests ()
end 

module Round : sig
  type t
  type status = Pending | Running | Done | Cancelled
  
  val make : t
  val resurect : db_file:string -> t

  val run : t -> t
  val stop : t -> t

  val status : t -> status
  val date : t -> string
  val digest : t -> string

  val list_problems : t -> Problem.t list Lwt.t
  val nb_tests : t -> int Lwt.t
  val nb_successful_tests : t -> int Lwt.t
end = struct
  type t_info = {
    timestamp: float;
  }
  type t_running = {
    info: t_info;
    process: Lwt_process.process_none;
  }
  type t_done = {
    info: t_info;
    db_file: string;
    rc: Unix.process_status Lwt.t
  }
  type t = 
    | S_pending of t_info
    | S_running of t_running
    | S_done of t_done
    | S_cancelled of t_info

  type status = Pending | Running | Done | Cancelled

  let make = 
    let t = Unix.time () in
    S_pending {timestamp=t}

  let resurect ~db_file = 
    let timestamp = Unix.time () in 
    let info = {timestamp} in
    S_done {info; db_file = db_file; rc = Lwt.return (Unix.WEXITED 0)}

  let run = function
    | S_pending i -> 
        let command = ("benchpress", [||]) in
        let process = Lwt_process.open_process_none command in
        S_running {info=i; process}
    | S_running _ | S_done _ | S_cancelled _ as round -> round 

  let stop = function
    | S_pending i ->
        let rc = Lwt.return (Unix.WSTOPPED 0) in
        S_done {info=i; db_file=""; rc}
    | S_running {info; process} ->
        process#terminate; 
        S_done {info; db_file=""; rc=process#close}
    | S_done _ | S_cancelled _ as round -> round

  let info = function 
    | S_pending i -> i
    | S_running r -> r.info
    | S_done d -> d.info
    | S_cancelled i -> i

  let status = function
    | S_pending _ -> Pending
    | S_running _ -> Running
    | S_done _ -> Done
    | S_cancelled _ -> Cancelled

  let date round = 
    let tm = Unix.localtime (info round).timestamp in
    Format.sprintf "le %i/%02i/%04i à %i:%02i:%02i"
      tm.tm_mday tm.tm_mon tm.tm_year
      tm.tm_hour tm.tm_min tm.tm_sec 

  let digest round = 
    let str = match round with
    | S_pending i -> string_of_float i.timestamp
    | S_running r -> string_of_float r.info.timestamp 
    | S_done d -> d.db_file ^ (string_of_float d.info.timestamp)
    | S_cancelled _ -> ""
    in
    str |> Digest.string |> Digest.to_hex

  let connect = function 
    | S_pending _ | S_running _ | S_cancelled _ -> 
        Lwt.return None
    | S_done d -> 
      let db_uri = 
        "sqlite3://" ^ Filename.concat benchpress_share_dir d.db_file 
        |> Uri.of_string
      in
      match%lwt Caqti_lwt.connect db_uri with
      | Ok con -> Lwt.return (Some con)
      | Error err ->
          Dream.log "%a" Caqti_error.pp err;
          Lwt.return None

  let nb_tests round =
    match%lwt connect round with
    | Some con ->
        begin match%lwt Request.count_tests con with
        | Ok n -> Lwt.return n
        | Error _ -> Lwt.return 0 
        end
    | None -> Lwt.return 0

  let nb_successful_tests round =
    match%lwt connect round with
    | Some con ->
        begin match%lwt Request.count_successful_tests con with
        | Ok n -> Lwt.return n
        | Error _ -> Lwt.return 0 
        end
    | None -> Lwt.return 0

  let list_problems round = 
    match%lwt connect round with
    | Some con -> 
        begin match%lwt Request.select_problems con with
        | Ok pbs -> Lwt.return pbs
        | Error err ->
            Dream.log "%a" Caqti_error.pp err;
            Lwt.return []
        end
    | None -> Lwt.return []
end

module Views : sig
  val render_404_not_found : Dream.request -> string Lwt.t
  val render_rounds_list : Round.t list -> Dream.request -> string Lwt.t
  val render_round_detail : Round.t -> Dream.request -> string Lwt.t
  val render_problem_detail : Problem.t -> Dream.request -> string Lwt.t
end = struct
  let html_to_string html = 
    Lwt.return @@ Format.asprintf "%a@." (Tyxml.Html.pp ~indent:true ()) html

  let page_layout ~subtitle ~content =
    let open Tyxml.Html in
    let str = Format.sprintf "Benchtop -- %s" subtitle in
    let css_custom_path = "./css/custom.css" in
    let css_bootstrap_url = "https://cdn.jsdelivr.net/npm\
      /bootstrap@4.0.0/dist/css/bootstrap.min.css"
    in 
    let hash = "sha384-Gn5384xqQ1aoWXA\
      +058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm"
    in
    html (head (title (txt str)) [
        meta ~a:[a_charset "utf-8"] ()
      ; meta ~a:[
          a_name "viewport"
        ; a_content "with=device-width, init-scale=1"
      ] () 
      ; link ~a:[a_integrity hash; a_crossorigin `Anonymous]
        ~rel:[`Stylesheet] ~href:css_bootstrap_url ()
      ; link ~rel:[`Stylesheet] ~href:css_custom_path ()
    ]) (body content) 

  let render_404_not_found request =
    page_layout ~subtitle:"404 not found" 
      ~content:[Tyxml.Html.txt "404 not found"]
    |> html_to_string

  let round_row ~number ~date ~result ~status ~action_button =
    let open Tyxml.Html in
    tr [
        td [txt (string_of_int number)]
      ; td ~a:[a_class ["text-center"]] [txt date]
      ; td ~a:[a_class ["text-center"; snd result]] [fst result]
      ; td ~a:[a_class ["text-center"]] [txt status] 
      ; td ~a:[a_class ["text-center"]] [action_button] 
    ] 

  let round_result round =
    let open Tyxml.Html in
    match Round.status round with
    | Pending -> Lwt.return (txt "Not yet", "bg-info")
    | Running -> Lwt.return (txt "Not yet", "bg-info")
    | Done ->
        let%lwt nb = Round.nb_tests round in
        let%lwt nb_successes = Round.nb_successful_tests round in
        let str = Format.sprintf "%i/%i" nb_successes nb in
        let color = 
          if nb_successes = nb then "bg-success"
          else "bg-danger"
        in
        Lwt.return (txt str, color)

  let round_action_button round = 
    let open Tyxml.Html in
    let formaction, action, color_button =
      let digest = Round.digest round in 
      match Round.status round with
      | Pending -> 
          ("cancel/" ^ digest, "Cancel", "btn-warning") 
      | Running -> 
          ("stop/" ^ digest, "Stop", "btn-danger")
      | Done -> 
          ("show/" ^ digest, "Show", "btn-primary")
    in
    button ~a:[
        a_button_type `Submit
      ; a_formaction formaction
      ; a_class ["btn"; color_button]] [txt action] 

  let rounds_table rounds =
    let open Tyxml.Html in
    let%lwt rows = rounds |> Lwt_list.mapi_s (fun i round ->
      let date = Round.date round in
      let status, formaction, action, color_button =
        let digest = Round.digest round in 
        match Round.status round with
        | Pending -> 
            ("Pending", "cancel/" ^ digest, "Cancel", "btn-warning") 
        | Running -> 
            ("Running", "stop/" ^ digest, "Stop", "btn-danger")
        | Done -> 
            ("Done", "show/" ^ digest, "Show", "btn-primary")
      in
      let%lwt result = round_result round in
      let action_button = round_action_button round in
      Lwt.return @@ round_row ~number:i ~date ~result ~status ~action_button)
     in
    Lwt.return @@ tablex ~a:[a_class ["table"]] 
      ~thead:(thead [
        tr [
          th [txt "#"]
        ; th ~a:[a_class ["text-center"]] [txt "Date"]
        ; th ~a:[a_class ["text-center"]] [txt "Result"]
        ; th ~a:[a_class ["text-center"]] [txt "Status"]
        ; th ~a:[a_class ["text-center"]] [txt "Action"]
      ]
      ]) [tbody rows] 

  let render_rounds_list rounds request =
    let%lwt rounds_table = rounds_table rounds in
    let form = Tyxml.Html.( 
      form ~a:[a_method `Get] [rounds_table]
    ) in
    page_layout ~subtitle:"Rounds" ~content:[form] 
    |> html_to_string

  let problem_row ~number ~pb_file ~result ~expected_result =
    let open Tyxml.Html in
    tr [
        th [txt (string_of_int number)]
      ; td ~a:[a_class ["text-left"]] 
          [txt @@ pb_file]
      ; td ~a:[a_class ["text-center"]] 
          [txt @@ string_of_result result]
      ; td ~a:[a_class ["text-center"]] 
          [txt @@ string_of_result expected_result]
    ]

  let problems_table pbs =
    let open Tyxml.Html in
    let rows = pbs |> List.mapi (fun i pb ->
      let open Problem in
      problem_row ~number:i 
        ~pb_file:pb.file_path 
        ~result:pb.result
        ~expected_result:pb.expected_result
    ) in
    tablex ~a:[a_class ["table"]] 
      ~thead:(thead [
        tr [
          th [txt "#"]
        ; th ~a:[a_class ["text-left"]] [txt "Problem"]
        ; th ~a:[a_class ["text-center"]] [txt "Result"]
        ; th ~a:[a_class ["text-center"]] [txt "Expected"]
      ]
      ]) [tbody rows] 

  let render_round_detail round request =
    let%lwt pbs = Round.list_problems round in
    let table = problems_table pbs in
    page_layout ~subtitle:"Round" ~content:[table]
    |> html_to_string 

  let render_problem_detail pb request = 
    failwith "Not implemented yet."
end

module Handlers : sig
  val handle_rounds_list : Round.t list ref -> Dream.handler
  val handle_round_detail : Round.t list ref -> Dream.handler
end = struct 
  let handle_rounds_list rounds request = 
    let%lwt html = Views.render_rounds_list !rounds request in
    Dream.html html

  let handle_round_detail rounds request =
    let digest = Dream.param request "digest" in
    let%lwt response = match List.find_opt (fun round -> 
      Digest.equal (Round.digest round) digest
    ) !rounds with
    | Some round ->
      Views.render_round_detail round request 
    | None -> 
      Views.render_404_not_found request
    in
    Dream.html response
end

module File = struct
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
  let resurected_rounds =  
    File.readdir 
      ~ext_filter:(fun str -> String.equal str ".sqlite") 
      benchpress_share_dir 
    |> List.map (fun db_file -> Round.resurect ~db_file) 
  in
  let rounds = ref resurected_rounds in
  Dream.run 
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [
      Dream.get "/css/**" @@ Dream.static (List.hd Location.Sites.css)
    ; Dream.get "/" @@ Handlers.handle_rounds_list rounds
    ; Dream.get "/show/:digest" @@ Handlers.handle_round_detail rounds
  ] 
