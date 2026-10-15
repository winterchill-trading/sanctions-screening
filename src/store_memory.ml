open! Core
open Async_kernel

type t =
  { versions : (string * string, List_version.t) Hashtbl.t
  ; subjects : Subject.t String.Table.t
  ; results : Screening.Result.t String.Table.t
  ; result_keys : (string * string) String.Table.t (** key -> (result id, digest) *)
  ; events : Case.Event.t list String.Table.t (** reversed *)
  ; case_keys : (string * string) String.Table.t (** key -> (case id, digest) *)
  }

let create () =
  { versions = Hashtbl.Poly.create ()
  ; subjects = String.Table.create ()
  ; results = String.Table.create ()
  ; result_keys = String.Table.create ()
  ; events = String.Table.create ()
  ; case_keys = String.Table.create ()
  }
;;

let digest writer v = Md5.to_hex (Md5.digest_string (Bin_prot.Writer.to_string writer v))

let save_list_version t (v : List_version.t) =
  let key = List_id.to_string v.list, v.version in
  return
    (match Hashtbl.find t.versions key with
     | Some old when List_version.equal old v -> Ok `Duplicate
     | Some _ ->
       Or_error.error_s
         [%message "list version already saved with other content" ~version:v.version]
     | None ->
       Hashtbl.set t.versions ~key ~data:v;
       Ok `Created)
;;

let list_versions t list =
  Hashtbl.data t.versions
  |> List.filter ~f:(fun (v : List_version.t) -> List_id.equal v.list list)
  |> List.sort ~compare:(fun (a : List_version.t) b -> String.compare a.version b.version)
  |> Or_error.return
  |> return
;;

let save_subject t (s : Subject.t) =
  Hashtbl.set t.subjects ~key:s.id ~data:s;
  return (Ok ())
;;

let subjects t =
  return
    (Ok
       (Hashtbl.data t.subjects
        |> List.sort ~compare:(fun (a : Subject.t) b -> String.compare a.id b.id)))
;;

let save_result t ~idempotency_key (r : Screening.Result.t) =
  let d = digest Screening.Result.Stable.V1.bin_writer_t r in
  return
    (match Hashtbl.find t.result_keys idempotency_key with
     | Some (_, d') when String.equal d d' -> Ok `Duplicate
     | Some _ ->
       Or_error.error_s
         [%message "idempotency key reused with other content" idempotency_key]
     | None ->
       if Hashtbl.mem t.results r.id
       then Or_error.error_s [%message "screening result id already saved" ~id:r.id]
       else (
         Hashtbl.set t.result_keys ~key:idempotency_key ~data:(r.id, d);
         Hashtbl.set t.results ~key:r.id ~data:r;
         Ok `Created))
;;

let find_result t id = return (Ok (Hashtbl.find t.results id))

let latest_result t ~subject_id =
  Hashtbl.data t.results
  |> List.filter ~f:(fun (r : Screening.Result.t) -> String.equal r.subject.id subject_id)
  |> List.max_elt ~compare:(fun (a : Screening.Result.t) b ->
    [%compare: Time_ns.t * string] (a.at, a.id) (b.at, b.id))
  |> Or_error.return
  |> return
;;

let append_case_events t ~idempotency_key ~expected_version (events : Case.Event.t list) =
  return
    (match events with
     | [] -> Or_error.error_s [%message "no events to append"]
     | first :: _ ->
       let case_id = first.case_id in
       let d =
         digest (List.bin_writer_t Case.Event.Stable.V1.bin_writer_t) events
       in
       (match Hashtbl.find t.case_keys idempotency_key with
        | Some (c, d') when String.equal c case_id && String.equal d d' -> Ok `Duplicate
        | Some _ ->
          Or_error.error_s
            [%message "idempotency key reused with other content" idempotency_key]
        | None ->
          let stored = Option.value (Hashtbl.find t.events case_id) ~default:[] in
          let version =
            match stored with
            | [] -> 0
            | e :: _ -> e.seq
          in
          if version <> expected_version
          then
            Or_error.error_s
              [%message "case version conflict" case_id ~expected:(expected_version : int) ~stored:(version : int)]
          else if
            List.existsi events ~f:(fun i (e : Case.Event.t) ->
              (not (String.equal e.case_id case_id)) || e.seq <> expected_version + i + 1)
          then Or_error.error_s [%message "events out of sequence" case_id]
          else (
            Hashtbl.set t.events ~key:case_id ~data:(List.rev_append events stored);
            Hashtbl.set t.case_keys ~key:idempotency_key ~data:(case_id, d);
            Ok `Created)))
;;

let case_events t ~case_id =
  return (Ok (List.rev (Option.value (Hashtbl.find t.events case_id) ~default:[])))
;;

let open_case_ids t =
  Hashtbl.to_alist t.events
  |> List.filter_map ~f:(fun (id, rev) ->
    match Case.of_events (List.rev rev) with
    | Ok c when not (Case.Status.is_final c.status) -> Some id
    | Ok _ | Error _ -> None)
  |> List.sort ~compare:String.compare
  |> Or_error.return
  |> return
;;
