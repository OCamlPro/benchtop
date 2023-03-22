let cat fmt fl =
  let ch = open_in fl in
  try
    while true do
      let s = input_line ch in
      Format.fprintf fmt "%s@\n" s
    done
  with End_of_file -> Format.fprintf fmt "@."

let read_dir ?ext_filter path =
  let apply_filter =
    match ext_filter with
    | Some filter ->
        let test file = filter @@ Filename.extension file in
        List.filter test
    | None -> fun x -> x
  in
  Sys.readdir path |> Array.to_list |> apply_filter

let read_lines ?count file =
  let open Syntax in
  Lwt_unix.openfile file [O_RDONLY] 0o650
  >>= fun fd ->
    let cin = Lwt_io.of_fd ~mode:Input fd in
    let stream = Lwt_io.read_lines cin in
    match count with
    | Some c ->
        Lwt_stream.nget c stream
        >>= fun lst ->
          let* _ = Lwt_io.close cin in
          Lwt_result.return lst
    | None ->
        Lwt_stream.to_list stream
        >>= fun lst ->
          let* _ = Lwt_io.close cin in
          Lwt_result.return lst

let extract_zip_file file =
  let cin = Zip.open_in file in
  try
    match Zip.entries cin with
    | [entry] when not @@ entry.Zip.is_directory ->
      let content = Zip.read_entry cin entry in
      Zip.close_in cin;
      Ok (content)
    | _ ->
      Zip.close_in cin;
      Error (`Cannot_read file)
  with exn ->
    Zip.close_in cin;
    raise exn

let to_temp_file prefix suffix =
  let prefix, suffix = Format.(sprintf "%s_" prefix, sprintf "_%s" suffix) in
  let filename = Filename.temp_file prefix suffix in
  let ch = open_in filename in
  (filename, ch)
