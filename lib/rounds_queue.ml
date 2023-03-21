open Syntax

type t = { lst : (Round.t, Error.t) result list; pos : int option }

let to_list { lst; _ } = lst
let empty = { lst = []; pos = None }

let make ~dir =
  let ext_filter str = String.equal str ".sqlite" in
  File.read_dir ~ext_filter dir |> Lwt_list.map_s Round.resurect
  >>= fun rounds ->
  Lwt.return @@
    List.sort
       (fun round1 round2 ->
         match (round1, round2) with
         | Ok round1, Ok round2 -> Round.compare round2 round1
         | _ -> -1)
       rounds
  >|= fun lst -> { lst; pos = None }

let rec update { lst; pos } =
  let new_pos = ref pos in
  let lst =
    List.mapi
      (fun j (round : (Round.t, _) result) ->
        match (pos, round) with
        | Some i, Ok round when i = j ->
            begin match round.status with
              | Pending _ -> Round.run round
              | Done _ ->
                new_pos := if j > 0 then Some (j - 1) else None;
                Lwt_result.return round
              | Running _ ->
                Round.update round
            end
        | Some i, Error err when i = j ->
            new_pos := if j > 0 then Some (j - 1) else None;
            Lwt_result.fail err
        | _ -> Lwt.return round)
      lst
  in
  let* lst = Lwt.all lst in
  if pos <> !new_pos then update { lst; pos = !new_pos }
  else Lwt.return { lst; pos = !new_pos }

let push round { lst; pos } =
  let pos = match pos with Some i -> Some (i + 1) | None -> Some 0 in
  { lst = Ok round :: lst; pos }

let find_by_uuid { lst; _ } uuid =
  let lst =
    List.partition_map (function Ok x -> Left x | Error err -> Right err) lst
    |> fst
  in
  List.find_opt
    (fun (round : Round.t) ->
      match round.status with
      | Done { summary; _ } -> String.equal uuid summary.uuid
      | Pending _ | Running _ -> false)
    lst
  |> Option.to_result ~none:`Round_not_found
  |> Lwt.return

let is_running { lst; pos } =
  match pos with
  | Some i -> (
      match List.nth lst i with
      | Ok round -> not @@ Round.is_done round
      | Error _ -> false)
  | None -> false
