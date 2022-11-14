open Tyxml

module Helper : sig
  val html_to_string : Tyxml.Html.doc -> string
  val pp_date : Unix.tm Fmt.t
  val csrf_tag : Dream.request -> [> Html_types.input] Tyxml.Html.elt
 
  val string_of_int : int -> string
  val string_of_float : float -> string
  val string_of_errcode : Models.Errcode.t -> string
  val string_of_res : Models.Res.t -> string

  val color_of_res : Models.Res.t -> string

  val pp_prover : Models.Prover.t Fmt.t

  val display_error : Dream.request -> [> Html_types.div] Tyxml.Html.elt list
end = struct
  let display_error request =
    match Error.get_session request with
    | Some err ->
        [[%html "\
          <div class='modal fade' id='error-modal' aria-hidden='true' \
            aria-labelledby='error modal' tabindex='-1'>\
            <div class='modal-dialog'>\
              <div class='modal-content'>\
                <div class='modal-header'>\
                  <h5 class='modal-title'>Error</h5>\
                  <button type='button' class='btn-close' \
                    data-bs-dismiss='modal' aria-label='close'></button>\
                </div>\
                <div class='modal-body'>\
                  <p>\
                    " [Html.txt (Format.asprintf "%a" Error.pp err)] "\
                  </p>\
                </div>\
              </div>\
            </div>\
          </div>\
        "]]
      | None -> []

  let html_to_string html =
    Format.asprintf "%a@." (Html.pp ~indent:true ()) html

  (* TODO: move this function in Models module. *)
  let pp_date fmt (tm : Unix.tm) = 
    Format.fprintf fmt "%02i/%02i/%04i %02i:%02i:%02i"
      tm.tm_mday (tm.tm_mon + 1) (tm.tm_year + 1900)
      tm.tm_hour tm.tm_min tm.tm_sec 

  let csrf_tag request =
    let token = Dream.csrf_token request in
    [%html "<input name='dream.csrf' type='hidden' value='" token "'/>"]

  let string_of_errcode (errcode : Models.Errcode.t) =
    match errcode with
    | Success -> "success"
    | Failed rc -> Format.sprintf "failed %i" rc

  let string_of_res (res : Models.Res.t) = Models.Res.to_string res
  let string_of_int = Format.sprintf "%i"
  let string_of_float = Format.sprintf "%f"

  let color_of_res (res : Models.Res.t) =
    match res with
    | Sat -> "text-success"
    | Unsat -> "text-primary"
    | Unknown | Timeout -> "text-orange"
    | Error | Unexpected _ -> "text-danger"

  (* TODO: move this function in Models module. *)
  let pp_prover fmt (prover : Models.Prover.t) =
    if String.length prover.version > 0 then
      Format.fprintf fmt "%s: %s" prover.name prover.version
    else
      Format.fprintf fmt "%s" prover.name
end 

let header_navbar ?(info = []) content =
  [%html "\
    <div class='container-fluid d-wrap flex-row flex-wrap'>\
      <a class='navbar-brand text-primary' href='#'>Benchtop</a>\
      <button class='navbar-toggler' type='button' \
        data-bs-toggle='collapse' data-bs-target='#collapse-content' \
        aria-controls='collapse-content' aria-expanded='false' \
        aria-label='Toggle collapse content'>\
        <span class='navbar-toggler-icon'></span>\
      </button>\
      <div class='collapse navbar-collapse \
        justify-content-center' id='collapse-content'>\
        " content "\
      </div>\
      <div class='flex-lg-grow-0 flex-grow-1 justify-content-center \
        text-right text-primary pe-4'>\
        " info "\
      </div>\ 
    </div>\
  "]

let footer_navbar content =
  [%html "\
    <div class='container-fluid d-wrap flex-row flex-wrap'>\
    " content "\
    </div>\
  "]

let page_layout request ~subtitle ?(hcontent=[]) ?(fcontent=[]) content =
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
        <link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.14.0-beta2/css/bootstrap-select.min.css'\ 
          integrity='sha512-mR/b5Y7FRsKqrYZou7uysnOdCIJib/7r5QeJMFvLNHNhtye3xJp1TdJVPLtetkukFn227nKpXD9OjUc09lx97Q==' \
          crossorigin='anonymous'/>\
        <script src='"bs_script_url"' integrity='"bs_script_hash"' \
          crossorigin='anonymous'></script>\
        <script src='scripts/modal.js'></script>\
        <script src='https://code.jquery.com/jquery-3.6.0.min.js' \
          integrity='sha256-/xUj+3OJU5yExlq6GSYGSHk7tPXikynS7ogEvDej/m4=' \ 
          crossorigin='anonymous'></script>
        <script src='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.14.0-beta2/js/bootstrap-select.min.js' \
        integrity='sha512-FHZVRMUW9FsXobt+ONiix6Z0tIkxvQfxtCSirkKc5Sb4TKHmqq1dZa8DphF0XqKb3ldLu/wgMa8mT6uXiLlRlw==' \
          crossorigin='anonymous'></script>
        <title>" (Html.txt str) "</title>\
     </head>\
      <body>\
        " (Helper.display_error request) "\
        <header class='navbar navbar-expand-lg navbar-light \
          bg-light sticky-top border-bottom'>\
          " hcontent "\
        </header>\
        <main>\
          " content "\
        </main>\
        <footer class='navbar navbar-expand-lg navbar-light bg-light \
          sticky-bottom border-top'>\
          " fcontent "\
        </footer>\
      </body>\
    </html>\
  "]

let check_selector ~number value =
  let num = string_of_int number in
  let id = "item_" ^ num in
  [%html "\
    <input class='form-check-input' type='checkbox' form='action-form' \
      id='"id"' name='"id"' value='"value"'/>\
  "]

let pagination ~current_uri ~limit ~page ~total =
  let number_of_pages = total / limit in
  let current_uri = Uri.remove_query_param current_uri "page" in
  let uri_of_page page =
    Uri.add_query_param' current_uri ("page", string_of_int page)
    |> Uri.to_string
  in
  let prev_or_next_link ~symbol ~page ~is_prev =
    let page, disabled = 
      if is_prev && page > 0 then
        (page - 1, "")
      else if not is_prev && page < number_of_pages then
        (page + 1, "")
      else (page, "disabled")
    in
    let aria_label = if is_prev then "previous" else "next" in
    [%html "\
      <li class='" ["page-item"; disabled] "'>\
        <a class='page-link' href='" (uri_of_page page) "' \
          aria-label='" [aria_label] "'>\
          <span aria-hidden='true'>" [Html.txt symbol] "</span>\
        </a>\
      </li>\
    "]
  in
  let page_link ~is_activated page =
    let params = 
      if is_activated then ["active"]
      else []
    in
    [%html "\
      <li class='" ("page-item" :: params) "'>\
        <a class='page-link' href='" (uri_of_page page) "'>\
        " [Html.txt (string_of_int page)] "\
        </a>\
      </li>\
    "]
  in
  let ellipsis = 
    [%html "\
      <li class='page-item disabled'>\
        <a class='page-link'>...</a>\
      </li>\
    "]
  in
  let links =
    let lst = 
      ref [prev_or_next_link ~symbol:"❱" ~page ~is_prev:false]
    in
    (* TODO: clean this code. *)
    for i = number_of_pages downto 0 do
      let is_activated = i = page in
      if (((i = 1 && page <> 0) || (i = number_of_pages-1 
        && page <> number_of_pages)) && not is_activated) then
        lst := ellipsis :: !lst
      else if i = 0 || abs(i - page) < 2 || i = number_of_pages then
        lst := page_link ~is_activated i :: !lst
    done;
    lst := prev_or_next_link ~symbol:"❰" ~page ~is_prev:true :: !lst;
    !lst
  in
  let info = 
    let offset = page * limit + 1 in
    let last =
      let n = offset + limit - 1 in
      if n < total then n else total
    in
    Format.sprintf "%i-%i of %i" 
    offset
    last
    total
  in
  [%html "\
    <nav aria-label='pagination'>\
      <ul class='pagination'>\
        " links "\
      </ul>\
      <span>\
      " [ Html.txt info ] "\
      </span>\
    </nav>\
  "]

(* BUG: placeholder option does not work properly. *)
module Selector = struct
  type default_option =
    | Default_value of {key: string; value: string}
    | Placeholder of string
    | None

  let make ~id ~label ?(default_option=None) ?(multiple=false) options request = 
    let currents = Misc.look_up_get_params request id in
    let options = List.map (fun (key, value) ->
      let attributes = 
        if List.mem key currents then
          Html.[a_value value; a_selected ()]
        else
          Html.[a_value value]
      in
      Html.(option ~a:attributes (txt key))
    ) options in
    let options =
      let selected_default = match currents with
      | [] -> [Html.a_selected ()]
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
    let select_attributes =
      let open Html in
      if multiple then
        [a_class ["form-control"; "mr-sm-2"; "selectpicker"]; a_id id; a_name id; a_multiple ()]
      else
        [a_class ["form-control"; "mr-sm-2"]; a_id id; a_name id]
    in
    [%html "\
      <div class='input-group'>\
        <label class='input-group-text' for='"id"'>\
          " [Html.txt label] "\
        </label>\
        " [Html.select ~a:select_attributes options] "\
      </div>\
    "]
end

let action_form ~actions request =
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
  Html.button ~a:attributes content

let checkbox ?(checked=false) ?(cla=[]) id =
  let open Tyxml.Html in
  let checked_attribut = 
    if checked then [a_checked ()] else []
  in
  input ~a:(checked_attribut @
    [a_input_type `Checkbox; a_id id; a_name id; a_class cla]) ()

let benchpress_form request ~is_running provers =
  let provers = List.map (fun prover ->
    let key = Format.asprintf "%a" Helper.pp_prover prover in
    let value =
      Models.Prover.to_yojson prover 
      |> Yojson.Safe.to_string
    in
    (key, value)
  ) provers in
  [%html "\
  <form class='d-flex flex-lg-row flex-column align-items-lg-center' \
    method='post' name='benchpress-controller' action='/benchpress/schedule'>\
    " [Helper.csrf_tag request] "\
    <div class='p-2'>\
      " [Selector.make ~id:"prover" ~label:"Prover"
          ~default_option:(Placeholder "...")
          provers request] "\
    </div>\
    <div class='p-2' hidden>\
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
  val table : (Round.t, Error.t) result list -> [> Html_types.tablex] Tyxml.Html.elt
  val action_form : Dream.request -> [> Html_types.form] Tyxml.Html.elt
end = struct 
  let format_status (round : Round.t) = 
    match round with
    | Pending _ -> Html.txt "Pending"
    | Running _ -> Html.txt "Running"
    | Done {summary; _} ->
        let url = "round/" ^ summary.uuid in
        Html.(a ~a:[a_href url] [txt "Done"])
 
  let format_date (round : Round.t) =
    let date =
      match round with
      | Pending {pending_since; _} -> pending_since
      | Running {running_since; _} -> running_since
      | Done {done_since; _} -> done_since
    in 
    Fmt.str "since %a" Helper.pp_date date 
 
  let format_result (round : Round.t) =
    match round with
    | Pending _ | Running _ -> Html.txt "Not yet"
    | Done {summary; _} ->
        let str = Format.sprintf "%i/%i" summary.ctr_suc_pbs summary.ctr_pbs in
        Html.txt str
 
  let row ~number round =
    match round with 
    | Ok (round : Round.t) -> begin
      let check_selector =
        match round with
        | Pending _ | Running _ -> []
        | Done {summary; _} ->
          [check_selector ~number (Dream.to_base64url summary.uuid)]
      in
      let prover = Round.prover round in
      [%html "\
        <tr>\
          <th>" check_selector "</th>\
          <td>" [Html.txt prover.name] "</td>\
          <td>" [Html.txt (Models.Prover.branch prover)] "</td>\
          <td>" [Html.txt (Models.Prover.hash prover)] "</td>\
          <td class='text-center'>" [format_result round] "</td>\
          <td class='text-center'>" [format_status round] "</td>\
          <td class='text-center'>" [Html.txt (format_date round)] "</td>\
        </tr>\
      "]
    end
    | Error _ -> 
      [%html "\
        <tr>\
          <th></th>\
          <td colspan='6'>\
            <span class='badge bg-danger'>Error</span>\
          </td>\
        </tr>\
      "]

  let table rounds =
    let rows = List.mapi (fun i round -> row ~number:i round ) rounds in
    [%html "\
      <table class='table table-striped table-hover align-middle\
        table-responsive'>\
        <thead>\
          <tr>\
            <th>Select</th>\
            <th>Prover</th>\
            <th>Branch</th>\
            <th>Hash</th>\
            <th class='text-center'>Result</th>\
            <th class='text-center'>Status</th>\
            <th class='text-center'>Date</th>\
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
 
let render_rounds_list request ~is_running rounds provers =
  let open Rounds_list in
  let rounds_table = table rounds in
  let navbar = header_navbar 
    [benchpress_form request ~is_running provers; action_form request]
  in
  page_layout request ~subtitle:"Rounds" ~hcontent:[navbar] [rounds_table]
  |> Helper.html_to_string

module Problems_list : sig
  val table:
    Dream.request ->
    Models.Problem.t list ->
    [> Html_types.tablex] Tyxml.Html.elt
  
  val action_form : Dream.request -> [> Html_types.form] Tyxml.Html.elt
  val filter_form : Dream.request -> [> Html_types.form] Tyxml.Html.elt
end = struct 
  let row request ~number pb =
    let open Models.Problem in
    let uuid = Dream.param request "uuid" in
    let pb_link = Format.sprintf 
      "/round/%s/problem/%s" uuid (Dream.to_base64url pb.file)
    in
    [%html "
      <tr>\
        <th>\ 
          " [check_selector ~number (Dream.to_base64url pb.file)] "\
        </th>\
        <td class='text-break'>\
          <a href='"pb_link"'>" [Html.txt pb.file] "</a>\
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
          '" ["text-center"; (Helper.color_of_res pb.file_expect)] "'>\
          " [Html.txt (Helper.string_of_res pb.file_expect)] "\
        </td>\
      </tr>\
    "]
  
  let table request pbs =
    let rows =  List.mapi (fun i pb -> row request ~number:i pb) pbs in
    [%html "\
      <table class='table table-striped table-hover align-middle \
        table-responsive'>\
        <thead>\
          <tr>\
            <th>Select</th>\
            <td>Problem</td>\
            <td class='text-center'>Timeout</td>\
            <td class='text-center'>Error code</td>\
            <td class='text-center'>Running time</td>\
            <td class='text-center'>Result</td>\
            <td class='text-center'>Expected</td>\
          </tr>\
        </thead>\
        <tbody class='table-group-divider'>\
        " rows "\
        </tbody>\
      </table>\
    "]

  let action_form =
    action_form ~actions:[("snapshot", "Snapshot")]

  let possible_results = [
    ("unsat", "unsat");
    ("sat", "sat");
    ("unknown", "unknown");
    ("error", "error");
    ("timeout", "timeout")
  ]

  let filter_form request =
    let checked = 
      Misc.look_up_get_opt_param request "only_diff" 
      |> Option.is_some
    in
    [%html "\
    <form class='d-flex flex-lg-row flex-column align-items-lg-center' \
      method='get'>\
      <div class='form-check form-switch p-2'>\
        <label class='form-check-label' for='only_diff'>\
          Diff\
        </label>\
        " [checkbox ~checked ~cla:["form-check-input"] 
            "only_diff"] "\
      </div>\
      <div class='p-2'>\
        <div class='input-group'>\
          <label class='input-group-text' for='file'>Problem</label>\
          <input type='text' class='form-control' id='file' name='file' \
            placeholder='...'/>\
        </div>\
      </div>\
      <div class='p-2'>\
      " [Selector.make ~multiple:true ~id:"errcode" ~label:"Error code"
          ~default_option:(Placeholder "")
          [("0", "0"); ("1", "1"); ("123", "123")] request]
      "\
     </div>\
     <div class='p-2'>\
      " [Selector.make ~multiple:true ~id:"res" ~label:"Result"
          ~default_option:(Placeholder "") 
          possible_results request]
      "\
     </div>\
     <div class='p-2'>\
      " [Selector.make ~multiple:true ~id:"file_expect" ~label:"Expected"
          ~default_option:(Placeholder "")
          possible_results request] "\
     </div>\
     <div class='p-2'>\
        <button class='btn btn-outline-success w-100' type='submit'>\
          Filter\
        </button>\
      </div>\
    </form>\
    "]
end

let round_summary _request ~prover =
  [Html.txt @@ Format.asprintf "%a" Helper.pp_prover prover]

let render_round_detail request ~page ~total ~prover
  (_summary : Models.Round_summary.t) pbs =
  let open Problems_list in
  let current_uri = Dream.target request |> Uri.of_string in
  let table = table request pbs  in
  let (header_navbar, footer_navbar) = (
      header_navbar ~info:(round_summary request ~prover) 
        [ filter_form request; action_form request ]
    , footer_navbar [pagination ~current_uri ~limit:50 ~page ~total])
  in
  page_layout
    request
    ~subtitle:"Round" 
    ~hcontent:[header_navbar] 
    ~fcontent:[footer_navbar] 
    [table]
  |> Helper.html_to_string

let render_problem_trace request (pb : Models.Problem.t) =
  let header = Format.sprintf "Problem %s" (Filename.basename pb.file) in
  (* BUG: we should recover if the file cannot be read. *)
  let problem_content = File.read_all (open_in pb.file) in
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
              " [Html.txt (Helper.string_of_res pb.file_expect)] "\
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
  page_layout request ~subtitle:"Problem trace" [content] 
  |> Helper.html_to_string

module Problem_diffs_list : sig
  val table:
    Dream.request ->
    prover_1:Models.Prover.t ->
    prover_2:Models.Prover.t ->
    Models.Problem_diff.t list ->
    [> Html_types.tablex] Tyxml.Html.elt
  
  val filter_form : Dream.request -> [> Html_types.form] Tyxml.Html.elt
end = struct
  let format_problem (pb : Models.Problem.t) =
    [%html "\
      <td>\
        " [Html.txt (Helper.string_of_errcode pb.errcode)] "\
      </td>\
      <td>\
        " [Html.txt (Helper.string_of_float pb.rtime)] "\
      </td>\
      <td class='" [Helper.color_of_res pb.res] "'>\
        " [Html.txt (Helper.string_of_res pb.res)] "\
      </td>\
    "]
 
  let row ~number 
    ((problem1, problem2) : (Models.Problem.t * Models.Problem.t)) _request =
    let pb_link = Format.sprintf 
      "/round//problem/%s" (Dream.to_base64url problem1.file)
    in
    [%html "
      <tr>\
        <th>" [check_selector ~number (Dream.to_base64url problem1.file)] "</th>\
        <td class='text-start text-break'>\
          <a href='"pb_link"'>" [Html.txt problem1.file] "</a>\
        </td>\
        <td class='" [Helper.color_of_res problem1.file_expect] "'>\
          " [Html.txt (Helper.string_of_res problem1.file_expect)] "\
        </td>\
        " ((format_problem problem1) @ (format_problem problem2)) "
      </tr>\
    "]

  let format_prover_header prover =
    Html.txt @@ Format.asprintf "Prover: %a" Helper.pp_prover prover
  
  let table request ~prover_1 ~prover_2 pb_diffs =
    let rows =  List.mapi (fun i pb_diff ->
      row ~number:i pb_diff request
    ) pb_diffs in
    [%html "\
      <table class='table table-striped table-hover align-middle \
        table-responsive text-center'>\
        <thead>\
          <tr>\
            <th colspan='3'></th>\
            <th colspan='3'>\
              " [format_prover_header prover_1] "\
            </th>\
            <th colspan='3'>\
              " [format_prover_header prover_2] "\
            </th>\
          </tr>
          <tr>\
            <th>Select</th>\
            <th class='text-left'>Problem</th>\
            <th>Expected</th>\
            <th>Error code</th>\
            <th>Running time</th>\
            <th>Result</th>\
            <th>Error code</th>\
            <th>Running time</th>\
            <th>Result</th>\
          </tr>\
        </thead>\
        <tbody class='table-group-divider'>\
        " rows "\
        </tbody>\
      </table>\
    "]

  let filter_form request =
    let checked = 
      Misc.look_up_get_opt_param request "show_rtime_reg" 
      |> Option.is_some
    in
    [%html "\
    <form class='d-flex flex-lg-row flex-column align-items-lg-center' \
      method='get'>\
      <div class='form-check form-switch p-2'>\
        <label class='form-check-label' for='show_rtime_reg'>\
          Show running time regression\
        </label>\
        " [checkbox ~checked ~cla:["form-check-input"] 
            "show_rtime_reg"] "\
      </div>\
      <div class='p-2'>\
        " [Selector.make ~id:"kind_diff" ~label:"Kind"
          ~default_option:(Default_value {key="difference"; value="difference"})
          [ ("improvement", "improvement"); 
            ("regression", "regression") ] request]
        "\
      </div>\
      <div class='p-2'>\
        <div class='input-group'>\
          <label class='input-group-text' for='name'>Problem</label>\
          <input type='text' class='form-control' id='file' name='file' \
            placeholder='...'/>\
        </div>\
      </div>\
      <div class='p-2'>\
        <button class='btn btn-outline-success w-100' type='submit'>\
          Filter\
        </button>\
      </div>\
   </form>\
   "]
end

let render_rounds_diff request ~page ~total ~prover_1 ~prover_2 pbs_diff =
  let open Problem_diffs_list in
  let current_uri = Dream.target request |> Uri.of_string in
  let (header_navbar, footer_navbar) = (
      header_navbar [filter_form request]
    , footer_navbar [pagination ~current_uri ~limit:50 ~page ~total])
  in
  page_layout 
    request
    ~subtitle:"Difference"
    ~hcontent:[header_navbar]
    ~fcontent:[footer_navbar]
    [table request ~prover_1 ~prover_2 pbs_diff]
  |> Helper.html_to_string
