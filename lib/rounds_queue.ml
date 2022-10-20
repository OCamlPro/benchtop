open Syntax

type t = {
  lst: (Round.t, Error.t) result list;
  pos: int option
}

let to_list {lst; _} = lst

let make ~dir =
  let ext_filter = fun str -> String.equal str ".sqlite" in
  File.readdir ~ext_filter dir
  |> Lwt_list.map_s (fun db_file -> Round.resurect ~db_file)
  >|= fun lst -> {lst; pos = None}

let rec update {lst; pos} =
  let new_pos = ref pos in
  (*Lwt_list.mapi_s (fun j round ->
    Result.bind round 
    (fun (round : Round.t) -> match (pos, round.status) with
    | Some i, Pending _ when i = j ->
        Round.run round
    | Some i, Done _ when i = j ->
        new_pos := if i > 0 then Some (i-1) else None;
        Round.update round
    | _ -> Round.update round)) lst
  >>= fun lst ->
    if pos <> !new_pos then
      update {lst; pos = !new_pos}
    else*)
      Lwt_result.return {lst; pos = !new_pos}

let push round {lst; pos} =
  let pos = 
    match pos with 
    | Some i -> Some (i+1)
    | None -> Some 0
  in
  {lst = (Ok round) :: lst; pos}

let find_by_uuid uuid {lst; _} =
  let opt = List.find_opt (fun round ->
    match round with
    | Ok (round : Round.t) -> begin
        match round.status with
        | Done {summary; _} -> String.equal uuid summary.uuid
        | Pending _ | Running _ -> false
      end
    | Error _ -> false) lst
  in
  match opt with
  | Some res -> res 
  | None -> Error `Not_found

let is_running {lst; pos} =
  match pos with
  | Some i -> begin
      match List.nth lst i with
      | Ok round -> not @@ Round.is_done round
      | Error _ -> false
    end
  | None -> false
