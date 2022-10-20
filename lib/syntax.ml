include Lwt.Infix
include Lwt.Syntax

let (>>?) = Lwt_result.Infix.(>>=)
let (>|?) = Lwt_result.Infix.(>|=)
let (>>!) = Lwt_result.bind_error
let (>|!) f g =
  let h x = Lwt_result.fail @@ g x in
  Lwt_result.bind_error f h
let (let*?) = Lwt_result.Syntax.(let*)
let (and*?) = Lwt_result.Syntax.(and*)
let (let+?) = Lwt_result.Syntax.(let+)
let (and+?) = Lwt_result.Syntax.(and+)
