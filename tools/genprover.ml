module Process : sig
  type cmd 
  type trace = private {
    stdout: in_channel;
    stderr: in_channel;
    rc: Unix.process_status
  }

  val create: bin:string -> args:string array -> cmd
  val bind: cmd -> cmd -> cmd
  val wait: cmd -> trace
end = struct
  type cmd = {
    bin: string;
    args: string array
  }
  
  type trace = private {
    stdout: in_channel;
    stderr: in_channel;
    rc: Unix.process_status
  }

  let create ~bin ~args = {bin; args}

end

module Git : sig
  type t

  val clone: t -> unit
  val fetch: t -> origin:string -> hash:string -> unit
end = struct
  type t = {
    dir: Fpath.t;
  }

  let clone rep = ()

  let fetch rep ~origin ~hash = ()
end

let () = ()
