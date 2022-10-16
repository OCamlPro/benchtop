type view = Dream.request -> Dream.response Lwt.t

module Helper : sig
  val html_to_string : Tyxml.Html.doc -> Dream.response Lwt.t
  val format_date : Unix.tm -> string
  val csrf_tag : Dream.request -> [> Html_types.input] Tyxml.Html.elt
 
  val string_of_int : int -> string
  val string_of_float : float -> string
  val string_of_errcode : Models.Fields.Errcode.t -> string
  val string_of_res : Models.Fields.Res.t -> string
  val string_of_ext : Models.Fields.Ext.t -> string

  val color_of_res : Models.Fields.Res.t -> string

  val pp_prover : Models.Prover.t Misc.printer
  val look_up_param : string -> Dream.request -> string
end = struct
  let look_up_param param request =
    let uri = Dream.target request |> Uri.of_string in
    Option.value ~default:"" (Uri.get_query_param uri param)
  
  let html_to_string html =
    Format.asprintf "%a@." (Tyxml.Html.pp ~indent:true ()) html
    |> Dream.html

  let format_date (tm : Unix.tm) = 
    Format.sprintf "%02i/%02i/%04i %02i:%02i:%02i"
      tm.tm_mday tm.tm_mon (tm.tm_year + 1900)
      tm.tm_hour tm.tm_min tm.tm_sec 

  let csrf_tag request =
    let open Tyxml in
    let token = Dream.csrf_token request in
    [%html "<input name='dream.csrf' type='hidden' value='" token "'/>"]

  let string_of_errcode (errcode : Models.Fields.Errcode.t) =
    match errcode with
    | Success -> "success" 
    | Failed rc -> Format.sprintf "failed %i" rc

  let string_of_res (res : Models.Fields.Res.t) =
    match res with
    | Sat -> "sat"
    | Unsat -> "unsat"
    | Unknown -> "unknown"
    | Error -> "error"

  let string_of_ext (ext : Models.Fields.Ext.t) =
    match ext with
    | Ae -> "ae"
    | Smt2 -> "smt2"
    | Psmt2 -> "psmt2"

  let string_of_int = Format.sprintf "%i"
  let string_of_float = Format.sprintf "%f"

  let color_of_res (res : Models.Fields.Res.t) =
    match res with
    | Sat -> "text-success"
    | Unsat -> "text-primary"
    | Unknown -> "text-orange"
    | Error -> "text-danger"

  let pp_prover fmt (prover : Models.Prover.t) =
    Format.fprintf fmt "%s: %s" prover.name prover.version
end 

let navbar ?(collapse_content=[]) content =
  let open Tyxml in
  [%html "\
    <nav class='container-fluid flex-wrap'>\
      <a class='navbar-brand text-primary' href='#'>Benchtop</a>\
      <button class='navbar-toggler' type='button' \
        data-bs-toggle='collapse' data-bs-target='#collapse-content' \
        aria-controls='collapse-content' aria-expanded='false' \
        aria-label='Toggle collapse content'>\
        <span class='navbar-toggler-icon'></span>\
      </button>\
      <div class='collapse navbar-collapse d-flex flex-row \
        justify-content-center' id='collapse-content'>\
        " collapse_content "\
      </div>\
      <div class='d-flex flex-row flex-grow-1 flex-lg-grow-0 \
        justify-content-center'>\
        " content "\
      </div>\
    </nav>\
  "]

let page_layout ~subtitle ?(hcontent=[]) ?(fcontent=[]) content =
  let open Tyxml in
  let str = Format.sprintf "Benchtop -- %s" subtitle in
  let bs_css_url = "https://cdn.jsdelivr.net/npm\
    /bootstrap@5.2.2/dist/css/bootstrap.min.css"
  in 
  let bs_css_hash ="sha384-Zenh87qX5JnK2Jl0vWa8Ck2rdkQ2Bzep5IDxbcnCeuOxjzrPF/\
    et3URy9Bv1WTRi"
  in
  let bs_script_url = "https://cdn.jsdelivr.net/npm\
    /bootstrap@5.2.2/dist/js/bootstrap.bundle.min.js"
  in
  let bs_script_hash = "sha384-OERcA2EqjJCMA+/\
    3y+gxIOqMEjwtxJY7qPCqsdltbNJuaOe923+mo//f6V8Qbsw3" 
  in
  [%html "\
    <html>\
      <head>\
        <meta charset='utf-8'/>\
        <meta name='viewport' content='with=device-width,init-scale=1'/>\
        <link integrity='"bs_css_hash"' crossorigin='anonymous' \
          rel='stylesheet' href='"bs_css_url"'/>\
        <script src='"bs_script_url"' integrity='"bs_script_hash"' \
          crossorigin='anonymous'></script>\
        <title>" (Html.txt str) "</title>\
      </head>\
      <body>\
        <header class='navbar navbar-expand-lg navbar-light \
          bg-light sticky-top'>\
          " hcontent "\
        </header>\
        <main>\
          " content "\
        </main>\
        <footer>\
          " fcontent "\
        </footer>\
      </body>\
    </html>\
  "]

(*let vertical_rule = 
  let open Tyxml in
  [%html {eos|<div class="vr"></div>|eos}]*)

let render_404_not_found _request =
  page_layout ~subtitle:"404 not found" [Tyxml.Html.txt "404 not found"]
  |> Helper.html_to_string

let check_selector ~number value =
  let open Tyxml in
  let num = string_of_int number in
  let id = "item_" ^ num in
  [%html "\
    <input class='form-check-input' type='checkbox' form='action-form' \
      id='"id"' name='"id"' value='"value"'/>\
  "]

module Selector = struct
  type default_option =
    | Default_value of {key: string; value: string}
    | Placeholder of string
    | None

  let make ~id ~label ?(default_option=None) options request = 
    let open Tyxml in
    let current = Helper.look_up_param id request in
    let options = List.map (fun (key1, value) ->
      let attributes = match current with
      | key2 when String.equal key1 key2 ->
          Html.[a_value value; a_selected ()]
      | _ ->
          Html.[a_value value]
      in
      Html.(option ~a:attributes (txt key1))
    ) options in
    let options =
      let selected_default = match current with
      | "" -> [Html.a_selected ()]
      | _ -> []
      in
      match default_option with
      | Default_value {key; value} ->
          Html.(option ~a:(a_value value :: selected_default) (txt key))
            :: options
      | Placeholder key ->
          Html.(option
            ~a:([a_disabled(); a_hidden (); a_value ""] @ selected_default)
            (txt key)) :: options
      | None -> options
    in
    [%html "\
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
  [%html "\
    <form class='d-flex flex-column flex-lg-row align-items-lg-center' \
      name='action-form' id='action-form' \
      method='post' action='round/action'>\
      " [Helper.csrf_tag request] "\
      <div class='p-2'>\
        " [Selector.make ~id:"action_kind" ~label:"Action"
            ~default_option:(Placeholder "...") actions request] "\
      </div>\
      <button class='btn btn-outline-success m-2' type='submit'>\
        Do\
      </button>\
    </form>\
  "]

let button ?(disabled=false) ~ty ~cla ~formaction content =
  let open Tyxml.Html in
  let attributes = [
      a_class cla
    ; a_button_type ty
    ; a_formaction formaction
  ] in
  let attributes =
    if disabled then attributes
    else a_disabled () :: attributes
  in
  Tyxml.Html.button ~a:attributes content

let benchpress_form ~is_running request =
  let open Tyxml in
  [%html "\
  <form class='d-flex flex-lg-row flex-column align-items-lg-center' \
    method='post' name='benchpress-controller' action='/benchpress/schedule'>\
    " [Helper.csrf_tag request] "\
    <div class='p-2'>\
      " [Selector.make ~id:"prover" ~label:"Prover"
          ~default_option:(Default_value {key="default"; value="default"})
          [] request] "\
    </div>\
    <div class='p-2'>\
      " [Selector.make ~id:"config" ~label:"Config"
          ~default_option:(Default_value {key="default"; value="default"})
          [] request] "\
    </div>\
    <div class='p-2'>\
      <button class='btn btn-outline-success w-100' type='submit'>\
        Schedule\
      </button>\
    </div>\
    <div class='p-2'>\
      " [button ~disabled:is_running ~ty:`Submit
        ~cla:["btn"; "btn-danger"; "w-100"]
        ~formaction:"/benchpress/stop" [Html.txt "Stop"]] "\
    </div>\
  </form>\
  "]

module Rounds_list : sig
  val table : Round.t list -> [> Html_types.tablex] Tyxml.Html.elt
  val action_form : Dream.request -> [> Html_types.form] Tyxml.Html.elt
end = struct 
  let format_status (round : Round.t) =
    let open Tyxml.Html in
    match round.status with
    | Pending _  -> txt "Pending"
    | Running _  -> txt "Running"
    | Done {summary; _} ->
        let link = "round/" ^ summary.uuid in
        a ~a:[a_href link] [txt "Done"]
    | Failed _ -> txt "Error"
 
  let format_provers (round : Round.t) =
    let open Tyxml.Html in
    match round.status with
    | Pending _ | Running _ | Failed _ -> txt ""
    | Done {provers; _} ->
      (* let pp fmt el = Format.fprintf fmt "%s" el in *)
      (*let provers = List.map fst info.provers |> sprintf_list pp in*)
      txt (Misc.sprintf_list Helper.pp_prover provers)
 
  let format_uuid (round : Round.t) =
    let open Tyxml.Html in
    match round.status with
    | Pending _ | Running _ | Failed _ -> txt ""
    | Done {summary; _} ->
        txt summary.uuid
  
  let format_result (round : Round.t) =
    let open Tyxml.Html in
    match round.status with
    | Pending _ | Running _ -> txt "Not yet"
    | Done {summary; _} ->
        let str = Format.sprintf "%i/%i" summary.ctr_suc_pbs summary.ctr_pbs in
        txt str
    | Failed _ -> txt "Error"
 
  let row ~number (round : Round.t) =
    let open Tyxml in
    let date = round.date |> Helper.format_date in
    let check_selector =
      match round.status with
      | Pending _ | Running _ | Failed _ -> []
      | Done {db_file; _} ->
          [check_selector ~number (Dream.to_base64url db_file)]
    in
    [%html "\
      <tr>\
        <th>" check_selector "</th>\
        <td>" [format_provers round] "</td>\
        <td>" [format_uuid round] "</td>\
        <td class='text-center'>" [Html.txt round.config] "</td>\
        <td class='text-center'>" [Html.txt date] "</td>\
        <td class='text-center'>" [format_result round] "</td>\
        <td class='text-center'>" [format_status round] "</td>\
      </tr>\
    "]

  let table rounds =
    let open Tyxml in
    let rows = List.mapi (fun i round -> row ~number:i round ) rounds in
    [%html "\
      <table class='table table-striped table-hover align-middle\
        table-responsive'>\
        <thead>\
          <tr>\
            <th>Select</th>\
            <th>Prover</th>\
            <th>Uuid</th>\
            <th class='text-center'>Config</th>\
            <th class='text-center'>Date</th>\
            <th class='text-center'>Result</th>\
            <th class='text-center'>Status</th>\
          </tr>\
        </thead>\
        <tbody class='table-group-divider'>\
          " rows "\
        </tbody>\
      </table>\
    "]

  let action_form request =
    action_form request ~actions:[("compare", "compare")]
end
 
let render_rounds_list ~is_running rounds request =
  let open Rounds_list in
  let rounds_table = table rounds in
  let navbar = navbar
    ~collapse_content:[benchpress_form ~is_running request]
    [action_form request]
  in
  page_layout ~subtitle:"Rounds" ~hcontent:[navbar] [rounds_table]
  |> Helper.html_to_string

module Problems_list : sig
  val table : Models.Problem.t list -> Dream.request ->
    [> Html_types.tablex] Tyxml.Html.elt
  val action_form : Dream.request -> [> Html_types.form] Tyxml.Html.elt
  val filter_form : Dream.request -> [> Html_types.form] Tyxml.Html.elt
end = struct 
  let row ~number pb request =
    let open Tyxml in
    let open Models.Problem in
    let uuid = Dream.param request "uuid" in
    let pb_link = Format.sprintf 
      "/round/%s/problem/%s" uuid (Dream.to_base64url pb.name)
    in
    [%html "
      <tr>\
        <th>" [check_selector ~number (Dream.to_base64url pb.name)] "</th>\
        <td>\
          " [Html.txt (Format.sprintf "%s%s" 
            pb.prover_name pb.prover_version)] "\
        </td>\
        <td class='text-break'>\
          <a href='"pb_link"'>" [Html.txt pb.name] "</a>\
        </td>\
        <td class='text-center'>\
        </td>\
        <td class='text-center'>\
          " [Html.txt (Helper.string_of_int pb.timeout)] "\
        </td>\
        <td class='text-center'>\
          " [Html.txt (Helper.string_of_errcode pb.errcode)] "\
        </td>\
        <td class='text-center'>\
          " [Html.txt (Helper.string_of_float pb.rtime)] "\
        </td>\
        <td class='" ["text-center"; (Helper.color_of_res pb.res)] "'>\
          " [Html.txt (Helper.string_of_res pb.res)] "\
        </td>\
        <td class=\
          '" ["text-center"; (Helper.color_of_res pb.expected_res)] "'>\
          " [Html.txt (Helper.string_of_res pb.expected_res)] "\
        </td>\
      </tr>\
    "]
  
  let table pbs request =
    let open Tyxml.Html in
    let rows =  List.mapi (fun i pb ->
      row ~number:i pb request
    ) pbs in
    tablex ~a:[a_class ["table table-striped table-hover align-middle 
      table-responsive"]] 
      ~thead:(thead [
        tr [
          th [txt "Select"]
        ; th ~a:[a_class ["text-left"]] [txt "Prover"]
        ; th ~a:[a_class ["text-left"]] [txt "Problem"]
        ; th ~a:[a_class ["text-center"]] [txt "Extension"]
        ; th ~a:[a_class ["text-center"]] [txt "Timeout"]
        ; th ~a:[a_class ["text-center"]] [txt "Error code"]
        ; th ~a:[a_class ["text-center"]] [txt "Running time"]
        ; th ~a:[a_class ["text-center"]] [txt "Result"]
        ; th ~a:[a_class ["text-center"]] [txt "Expected"]
      ]
      ]) [tbody ~a:[a_class ["table-group-divider"]] rows]
  
  let action_form =
    action_form ~actions:[("snapshot", "Snapshot")]
 
  let checkbox ?(checked=false) ?(cla=[]) id =
    let open Tyxml.Html in
    let checked_attribut = 
      if checked then [a_checked ()] else []
    in
    input ~a:(checked_attribut @
      [a_input_type `Checkbox; a_id id; a_name id; a_class cla]) ()

  let filter_form request =
    let open Tyxml in
    let checked = Helper.look_up_param "only_diff" request <> "" in
    [%html "\
    <form class='d-flex flex-lg-row flex-column align-items-lg-center' \
      method='get'>\
      <div class='form-check form-switch col-lg-1 p-2'>\
        <label class='form-check-label' for='only_diff'>\
          Diff\
        </label>\
        " [checkbox ~checked ~cla:["form-check-input"; "float-end"] 
            "only_diff"] "\
      </div>\
      <div class='p-2'>\
          <div class='input-group'>\
            <label class='input-group-text' for='name'>Problem</label>\
            <input type='text' class='form-control' id='name' name='name' \
              placeholder='...'/>\
          </div>\
      </div>\
      <div class='p-2'>\
      " [Selector.make ~id:"errcode" ~label:"Error code" 
          ~default_option:(Default_value {key="any"; value=""}) [
          ("0", "0"); ("<> 0", "<> 0"); ("1", "1"); ("123", "123")
          ] request] "\
     </div>\
     <div class='p-2'>\
      " [Selector.make ~id:"res" ~label:"Result"
          ~default_option:(Default_value {key="any"; value=""}) [
          ("unsat", "unsat"); ("sat", "sat"); ("unknown", "unknown");
          ("error", "error")
          ] request] "\
     </div>\ 
     <div class='p-2'>\
      " [Selector.make ~id:"expected_res" ~label:"Expected"
          ~default_option:(Default_value {key="any"; value=""}) [
          ("unsat", "unsat"); ("sat", "sat"); ("unknown", "unknown");
          ("error", "error")
          ] request] "\
     </div>\
     <div class='p-2'>\
        <button class='btn btn-outline-success w-100' type='submit'>\
          Filter\
        </button>\
      </div>\
    </form>\
    "]
end

let render_round_detail pbs request =
  let open Problems_list in 
  let table = table pbs request in
  let navbar = navbar ~collapse_content:[filter_form request]
    [action_form request]
  in
  page_layout ~subtitle:"Round" ~hcontent:[navbar] [table]
  |> Helper.html_to_string

let render_problem_trace (pb : Models.Problem.t) _request =
  let open Tyxml in
  let header = Format.sprintf "Problem %s" (Filename.basename pb.name) in
  let problem_content = File.read_all (open_in pb.name) in
  let%html content = "\
    <div class='container-fluid'>\
      <div class='card'>\
        <div class='card-header'>\
          " [Html.txt header] "\
        </div>\
        <div class='card-body'>\
          <div class='container'>\
            <div class='row'>\
              Result :\
              " [Html.txt (Helper.string_of_res pb.res)] "\
              Expected result :\
              " [Html.txt (Helper.string_of_res pb.expected_res)] "\
              Timeout :\
              " [Html.txt (Helper.string_of_int pb.timeout)] "\
              Error code :\
              " [Html.txt (Helper.(string_of_errcode pb.errcode))] "\
            </div>
            <div class='row'>\
              <label for='problem' class='form-label'>Problem content</label>\
              <textarea class='form-control bg-light' id='problem' rows='20' \
              readonly>\
                " (Html.txt problem_content) "\
              </textarea>\
            </div>\
            <div class='row'>\
              <div class='col'>\
                <label for='stdout' class='form-label'>Standard output</label>\
                <textarea class='form-control text-white bg-dark' \
                  id='stdout' rows='15' readonly>\
                  " (Html.txt pb.stdout) "\
                </textarea>\
              </div>\
              <div class='col'>\
                <label for='stdout' class='form-label'>Error output</label>\
                <textarea class='form-control text-white bg-dark' \
                  id='stdout' rows='15' readonly>\
                  " (Html.txt pb.stderr) "\
                </textarea>\
              </div>\
            </div>\
          </div>\
        </div>\
      </div>\
    </div>\
  " in
  page_layout ~subtitle:"Problem trace" [content] 
  |> Helper.html_to_string
