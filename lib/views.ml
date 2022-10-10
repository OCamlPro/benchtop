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
    (*let provers = List.map fst info.provers |> sprintf_list pp in*)
    txt "" 

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

let color_of_result (res : Models.res) =
  match res with
  | Sat -> ["text-success"]
  | Unsat -> ["text-primary"]
  | Unknown -> ["text-orange"]
  | Error -> ["text-danger"]

let problem_row ~number pb =
  let open Tyxml.Html in
  let open Models.Problem in
  tr [
      th (check_selector ~number) 
    ; td [txt @@ pb.pb_name]
    ; td ~a:[a_class ["text-center"]] 
        [txt @@ Models.string_of_ext pb.pb_ext]
    ; td ~a:[a_class ["text-center"]] 
        [txt @@ Models.string_of_int pb.timeout]
    ; td ~a:[a_class ["text-center"]] 
        [txt @@ Models.string_of_int @@ Models.int_of_error_code pb.error_code]
    ; td ~a:[a_class ["text-center"]] 
        [txt @@ Models.string_of_float pb.rtime]
    ; td ~a:[a_class (["text-center"] @ color_of_result pb.res)] 
        [txt @@ Models.string_of_result pb.res]
    ; td ~a:[a_class (["text-center"] @ color_of_result pb.expected_res)] 
        [txt @@ Models.string_of_result pb.expected_res]
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


