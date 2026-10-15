open! Core

type t =
  { snapshots : Snapshot.t list
  ; index : Index.t
  ; by_key : Entry.t String.Table.t
  }

let create snapshots =
  let by_key = String.Table.create () in
  let dup =
    List.find_map snapshots ~f:(fun (s : Snapshot.t) ->
      List.find_map s.entries ~f:(fun e ->
        match Hashtbl.add by_key ~key:(Entry.key e) ~data:e with
        | `Ok -> None
        | `Duplicate -> Some (Entry.key e)))
  in
  match dup with
  | Some key -> Or_error.error_s [%message "entry in two snapshots" key]
  | None ->
    let entries = List.concat_map snapshots ~f:(fun (s : Snapshot.t) -> s.entries) in
    Ok { snapshots; index = Index.create entries; by_key }
;;

let snapshots t = t.snapshots
let versions t = List.map t.snapshots ~f:(fun (s : Snapshot.t) -> s.version)
let index t = t.index
let find t key = Hashtbl.find t.by_key key
let entry_count t = Hashtbl.length t.by_key
