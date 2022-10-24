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

let update {lst; pos} =
  let new_pos = ref pos in
  let lst = List.mapi (fun j round ->
    Lwt.return round >>? fun ({status; _} as round : Round.t) ->
      match (pos, status) with 
      | Some i, Pending when i = j ->
          Round.run round
      | Some i, Done _ when i = j ->
          new_pos := if i > 0 then Some (i-1) else None;
          Round.update round
      | _ -> Round.update round
  ) lst in
  let+ lst = Lwt.all lst in 
  {lst; pos = !new_pos}

let push round {lst; pos} =
  let pos = 
    match pos with 
    | Some i -> Some (i+1)
    | None -> Some 0
  in
  {lst = (Ok round) :: lst; pos}

let find_by_uuid {lst; _} uuid =
  let lst = 
    List.partition_map (function 
    | Ok x -> Left x
    | Error err -> Right err) lst 
    |> fst
  in
  List.find_opt (fun (round : Round.t) ->
    match round.status with
      | Done {summary; _} -> String.equal uuid summary.uuid
      | Pending | Running _ -> false
  ) lst
  |> Option.to_result ~none:`Round_not_found
  |> Lwt.return

let is_running {lst; pos} =
  match pos with
  | Some i -> begin
      match List.nth lst i with
      | Ok round -> not @@ Round.is_done round
      | Error _ -> false
    end
  | None -> false
