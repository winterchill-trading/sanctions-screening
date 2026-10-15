open! Core

type t =
  { version : List_version.t
  ; entries : Entry.t list
  }
[@@deriving sexp_of]

let create ~list ~version ?published entries =
  let open Or_error.Let_syntax in
  let%bind () =
    match List.find entries ~f:(fun (e : Entry.t) -> not (List_id.equal e.list list)) with
    | None -> Ok ()
    | Some e ->
      Or_error.error_s
        [%message "entry from another list" ~id:e.id ~list:(e.list : List_id.t)]
  in
  let entries =
    List.sort entries ~compare:(fun (a : Entry.t) b -> String.compare a.id b.id)
  in
  let%bind () =
    match
      List.find_consecutive_duplicate entries ~equal:(fun (a : Entry.t) b ->
        String.equal a.id b.id)
    with
    | None -> Ok ()
    | Some (a, _) -> Or_error.error_s [%message "duplicate entry id" ~id:a.id]
  in
  let digest =
    let buf = Buffer.create 4096 in
    List.iter entries ~f:(fun e ->
      Buffer.add_string
        buf
        (Bin_prot.Writer.to_string Entry.Stable.V1.bin_writer_t { e with version = "" }));
    Md5.to_hex (Md5.digest_string (Buffer.contents buf))
  in
  return
    { version =
        { List_version.list; version; published; entries = List.length entries; digest }
    ; entries
    }
;;

module Diff = struct
  type t =
    { added : Entry.t list
    ; removed : Entry.t list
    ; changed : (Entry.t * Entry.t) list
    }
  [@@deriving sexp_of]

  let is_empty t =
    List.is_empty t.added && List.is_empty t.removed && List.is_empty t.changed
  ;;
end

let diff ~before ~after =
  let index t =
    String.Map.of_alist_exn (List.map t.entries ~f:(fun (e : Entry.t) -> e.id, e))
  in
  let b = index before
  and a = index after in
  Map.fold_symmetric_diff
    b
    a
    ~data_equal:Entry.same_content
    ~init:{ Diff.added = []; removed = []; changed = [] }
    ~f:(fun acc (_, change) ->
      match change with
      | `Left e -> { acc with removed = e :: acc.removed }
      | `Right e -> { acc with added = e :: acc.added }
      | `Unequal (x, y) -> { acc with changed = (x, y) :: acc.changed })
  |> fun (d : Diff.t) ->
  { Diff.added = List.rev d.added
  ; removed = List.rev d.removed
  ; changed = List.rev d.changed
  }
;;
