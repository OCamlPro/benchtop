type t = {
  lst: Round.t list;
  pos: int option
}

let to_list {lst; _} = lst

let make ~dir =
  let ext_filter = fun str -> String.equal str ".sqlite" in
  let%lwt lst =
    File.readdir ~ext_filter dir 
    |> Lwt_list.map_s (fun db_file -> Round.resurect ~db_file) 
  in
  Lwt.return {lst; pos = None}

let rec update {lst; pos} =
  let new_pos = ref pos in
  let%lwt lst = Lwt_list.mapi_s (fun j (round : Round.t) ->
    match (pos, round.status) with
    | Some i, Pending _ when i = j ->
        Round.run round
    | Some i, Done _ | Some i, Failed _ when i = j ->
        new_pos := if i > 0 then Some (i-1) else None;
        Round.update round
    | _ -> Round.update round) lst in
  if pos <> !new_pos then
    update {lst; pos = !new_pos}
  else
    Lwt.return {lst; pos = !new_pos}

let push round {lst; pos} =
  let pos = 
    match pos with 
    | Some i -> Some (i+1)
    | None -> Some 0
  in
  {lst = round :: lst; pos}

let find_by_uuid uuid {lst; _} = 
  List.find_opt (fun (round : Round.t) ->
    match round.status with
    | Done {info; _} -> String.equal uuid info.uuid
    | Pending _ | Running _ | Failed _ -> false) lst

let is_running {lst; pos} = 
  match pos with 
  | Some i -> not @@ Round.is_done (List.nth lst i)
  | None -> false
