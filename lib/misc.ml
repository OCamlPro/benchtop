type 'a printer = Format.formatter -> 'a -> unit

let pp_list pp fmt lst =
  let pp_sep fmt () = Format.fprintf fmt ",@," in 
  Format.pp_print_list ~pp_sep pp fmt lst

let pp_error_code fmt =
  let open Unix in
  function
  | WEXITED rc -> 
      Format.fprintf fmt "WEXITED %i" rc 
  | WSIGNALED rc -> 
      Format.fprintf fmt "WSIGNALED %i" rc 
  | WSTOPPED rc -> 
      Format.fprintf fmt "WSTOPPED %i" rc 

let sprintf_list pp lst =
  let buf = Buffer.create 200 in
  let fmt = Format.formatter_of_buffer buf in
  let pp_sep fmt () = Format.fprintf fmt ",@," in 
  Format.pp_print_list ~pp_sep pp fmt lst;
  Format.pp_print_flush fmt ();
  Buffer.to_seq buf |> String.of_seq
