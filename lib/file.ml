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

let to_temp_file prefix suffix =
  let (prefix, suffix) = 
    Format.(sprintf "%s_" prefix, sprintf "_%s" suffix) 
  in
  let filename = Filename.temp_file prefix suffix in
  let ch = open_in filename in
  (filename, ch)
