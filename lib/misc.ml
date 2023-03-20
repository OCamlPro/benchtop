open Syntax

let pp_list pp fmt lst =
  let pp_sep fmt () = Format.fprintf fmt ",@," in
  Format.pp_print_list ~pp_sep pp fmt lst

let pp_to_string pp el =
  pp Format.str_formatter el;
  Format.flush_str_formatter ()

let sprintf_list pp lst =
  let buf = Buffer.create 200 in
  let fmt = Format.formatter_of_buffer buf in
  let pp_sep fmt () = Format.fprintf fmt ",@," in
  Format.pp_print_list ~pp_sep pp fmt lst;
  Format.pp_print_flush fmt ();
  Buffer.to_seq buf |> String.of_seq

let from_base64url str =
  Dream.from_base64url str |> Option.to_result ~none:`Cannot_convert_to_base64

let now () = Unix.time () |> Unix.localtime

let look_up_get_opt_param request key =
  let uri = Dream.target request |> Uri.of_string in
  match Uri.get_query_param uri key with
  | Some value when value <> String.empty -> Some value
  | _ -> None

let assoc_all key1 =
  let rec aux acc = function
    | [] -> acc
    | (key2, lst) :: tl when key1 = key2 -> aux (lst @ acc) tl
    | _ :: tl -> aux acc tl
  in
  aux []

let look_up_get_params request key =
  let uri = Dream.target request |> Uri.of_string in
  Uri.query uri |> assoc_all key

let look_up_post_param request key =
  Dream.form request >|= function
  | `Ok params ->
      List.assoc_opt key params |> Option.to_result ~none:(`Key_not_found key)
  | `Expired x -> Error (`Expired x)
  | `Wrong_session x -> Error (`Wrong_session x)
  | `Invalid_token x -> Error (`Invalid_token x)
  | `Missing_token x -> Error (`Missing_token x)
  | `Many_tokens x -> Error (`Many_tokens x)
  | `Wrong_content_type -> Error `Wrong_content_type

let look_up_param request key =
  try Lwt_result.return (Dream.param request key)
  with Failure _ -> Lwt_result.fail (`Key_not_found key)
