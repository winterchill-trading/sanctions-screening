open! Core
open Async_kernel

module Make (S : Store.S) = struct
  let show label r ~f =
    match r with
    | Ok v -> printf "%s: %s\n" label (f v)
    | Error _ -> printf "%s: refused\n" label
  ;;

  let write w = Sexp.to_string [%sexp (w : Store.Write.t)]
  let at s = Time_ns.of_string_with_utc_offset s

  let lists =
    lazy
      (let entry =
         Entry.create
           ~list:(Other "conformance")
           ~id:"E-1"
           ~version:"v1"
           ~party:Person
           ~names:[ { name = "Oleg Paramonov"; kind = Primary } ]
           ~dobs:[ Year 1961 ]
           ()
         |> ok_exn
       in
       Lists.create
         [ Snapshot.create ~list:(Other "conformance") ~version:"v1" [ entry ] |> ok_exn ]
       |> ok_exn)
  ;;

  let subject id name = Subject.create ~id ~party:Person ~names:[ name ] () |> ok_exn

  let result ~time s =
    Screening.screen
      ~policy:Policy.default
      ~lists:(force lists)
      ~context:Onboarding
      ~at:(at time)
      s
  ;;

  let run t =
    (* List versions *)
    let v1 = List.hd_exn (Lists.versions (force lists)) in
    let v0 = { v1 with version = "v0"; entries = 0 } in
    let%bind r = S.save_list_version t v1 in
    show "save v1" r ~f:write;
    let%bind r = S.save_list_version t v1 in
    show "save v1 again" r ~f:write;
    let%bind r = S.save_list_version t { v1 with entries = 7 } in
    show "save v1 with other content" r ~f:write;
    let%bind r = S.save_list_version t v0 in
    show "save v0" r ~f:write;
    let%bind r = S.list_versions t (Other "conformance") in
    show "versions" r ~f:(fun vs ->
      String.concat ~sep:"," (List.map vs ~f:(fun (v : List_version.t) -> v.version)));
    let%bind r = S.list_versions t Ofac_sdn in
    show "versions of another list" r ~f:(fun vs -> Int.to_string (List.length vs));
    (* Subjects *)
    let%bind r = S.save_subject t (subject "c-2" "Maria Garcia") in
    show "save c-2" r ~f:(fun () -> "ok");
    let%bind r = S.save_subject t (subject "c-1" "Oleg Paramonov") in
    show "save c-1" r ~f:(fun () -> "ok");
    let%bind r = S.save_subject t (subject "c-2" "Maria Garcia Lopez") in
    show "replace c-2" r ~f:(fun () -> "ok");
    let%bind r = S.subjects t in
    show "subjects" r ~f:(fun ss ->
      String.concat
        ~sep:","
        (List.map ss ~f:(fun (s : Subject.t) -> s.id ^ "=" ^ List.hd_exn s.names)));
    (* Results *)
    let first = result ~time:"2026-10-15 10:00:00Z" (subject "c-1" "Oleg Paramonov") in
    let later = result ~time:"2026-10-15 11:00:00Z" (subject "c-1" "Oleg Paramonov") in
    let%bind r = S.save_result t ~idempotency_key:"k-1" first in
    show "save result" r ~f:write;
    let%bind r = S.save_result t ~idempotency_key:"k-1" first in
    show "save result again" r ~f:write;
    let%bind r = S.save_result t ~idempotency_key:"k-1" later in
    show "reuse key for another result" r ~f:write;
    let%bind r = S.save_result t ~idempotency_key:"k-2" later in
    show "save later result" r ~f:write;
    let%bind r = S.find_result t first.id in
    show "find result" r ~f:(fun o ->
      Bool.to_string ([%equal: Screening.Result.t option] o (Some first)));
    let%bind r = S.find_result t "missing" in
    show "find missing result" r ~f:(fun o -> Bool.to_string (Option.is_none o));
    let%bind r = S.latest_result t ~subject_id:"c-1" in
    show "latest result is the later one" r ~f:(fun o ->
      Bool.to_string ([%equal: Screening.Result.t option] o (Some later)));
    let%bind r = S.latest_result t ~subject_id:"c-9" in
    show "latest result of unknown subject" r ~f:(fun o -> Bool.to_string (Option.is_none o));
    (* Cases *)
    let case, opened = Case.open_ ~id:"case-1" ~at:(at "2026-10-15 12:00:00Z") first |> ok_exn in
    let%bind r = S.append_case_events t ~idempotency_key:"open-1" ~expected_version:0 opened in
    show "open case" r ~f:write;
    let%bind r = S.append_case_events t ~idempotency_key:"open-1" ~expected_version:0 opened in
    show "open case again" r ~f:write;
    let%bind r = S.append_case_events t ~idempotency_key:"open-1b" ~expected_version:0 opened in
    show "open case with another key" r ~f:write;
    let analyst = Case.Actor.Analyst "a" in
    let now = at "2026-10-15 12:05:00Z" in
    let case, assigned = Case.apply case ~actor:analyst ~at:now (Assign "a") |> ok_exn in
    let%bind r = S.append_case_events t ~idempotency_key:"cmd-1" ~expected_version:1 assigned in
    show "assign" r ~f:write;
    let%bind r = S.append_case_events t ~idempotency_key:"cmd-1x" ~expected_version:1 assigned in
    show "assign from a stale version" r ~f:write;
    let%bind r = S.open_case_ids t in
    show "open cases" r ~f:(String.concat ~sep:",");
    let case, decided =
      Case.apply
        case
        ~actor:analyst
        ~at:now
        (Decide { decision = True_positive; reason = "same passport" })
      |> ok_exn
    in
    let%bind r =
      S.append_case_events t ~idempotency_key:"cmd-2" ~expected_version:2 decided
    in
    show "decide" r ~f:write;
    let%bind r = S.case_events t ~case_id:"case-1" in
    show "events" r ~f:(fun es -> String.concat ~sep:"," (List.map es ~f:(fun e -> Int.to_string e.seq)));
    let%bind r = Store.find_case (module S) t "case-1" in
    show "case rebuilt" r ~f:(fun o -> Bool.to_string ([%equal: Case.t option] o (Some case)));
    let%bind r = S.open_case_ids t in
    show "open cases after closing" r ~f:(fun ids -> sprintf "[%s]" (String.concat ~sep:"," ids));
    let%map r = Store.find_case (module S) t "case-9" in
    show "unknown case" r ~f:(fun o -> Bool.to_string (Option.is_none o))
  ;;
end
