open! Core
open Sanctions_screening
open Fixture_lists

let result = lazy (screen (person ~dob:"1961-03-12" "Oleg Paramonov"))
let t0 = Time_ns.of_string_with_utc_offset "2026-10-15 13:00:00Z"
let a = Case.Actor.Analyst "analyst-a"
let b = Case.Actor.Analyst "analyst-b"

let step (case, log) ~actor cmd =
  match Case.apply case ~actor ~at:t0 cmd with
  | Ok (case, events) ->
    List.iter events ~f:(fun e -> print_s [%sexp (e.seq : int), (e.kind : Case.Event.Kind.t)]);
    case, log @ events
  | Error e ->
    print_s [%sexp "refused", (Error.sexp_of_t e : Sexp.t)];
    case, log
;;

let%expect_test "false positive needs a second analyst" =
  let case, events = Case.open_ ~id:"case-1" ~at:t0 (force result) |> ok_exn in
  print_s [%sexp (case.hits : string list)];
  let s = case, events in
  let s = step s ~actor:a (Assign "analyst-a") in
  let s = step s ~actor:b (Decide { decision = False_positive; reason = "different person" }) in
  let s = step s ~actor:a (Decide { decision = False_positive; reason = "different date of birth on passport" }) in
  let s = step s ~actor:a Approve in
  let s = step s ~actor:b Approve in
  let s = step s ~actor:b (Assign "analyst-b") in
  let s = step s ~actor:b (Note "customer file updated") in
  let case, log = s in
  print_s [%sexp (case.status : Case.Status.t)];
  print_s [%sexp (Case.equal (Case.of_events log |> ok_exn) case : bool)];
  [%expect {|
    (ofac-sdn:9001 uk-ofsi:15001 eu-consolidated:7001)
    (2 (Assigned analyst-a))
    (refused
     ("only the assigned analyst decides" (case case-1)
      (t.status (Under_review (analyst analyst-a)))
      (command (Decide (decision False_positive) (reason "different person")))
      (actor (Analyst analyst-b))))
    (3
     (Decided (decision False_positive)
      (reason "different date of birth on passport")))
    (4 Submitted_for_second_review)
    (refused
     ("the second review needs a different analyst" (case case-1)
      (t.status
       (Awaiting_second_review (decision False_positive)
        (reason "different date of birth on passport") (first analyst-a)))
      (command Approve) (actor (Analyst analyst-a))))
    (5 Approved)
    (6 Closed)
    (refused
     ("case is closed" (case case-1)
      (t.status
       (Closed (decision False_positive)
        (reason "different date of birth on passport") (decided_by analyst-a)
        (approved_by (analyst-b))))
      (command (Assign analyst-b)) (actor (Analyst analyst-b))))
    (7 (Noted "customer file updated"))
    (Closed (decision False_positive)
     (reason "different date of birth on passport") (decided_by analyst-a)
     (approved_by (analyst-b)))
    true
    |}]
;;

let%expect_test "true positive closes at once; rejection reopens" =
  let s = Case.open_ ~id:"case-2" ~at:t0 (force result) |> ok_exn in
  let s = step s ~actor:a (Escalate "same name, same date of birth") in
  let s = step s ~actor:a (Assign "analyst-a") in
  let s = step s ~actor:a (Decide { decision = True_positive; reason = "passport matches" }) in
  print_s [%sexp ((fst s).status : Case.Status.t)];
  let s = Case.open_ ~id:"case-3" ~four_eyes:All_decisions ~at:t0 (force result) |> ok_exn in
  let s = step s ~actor:a (Assign "analyst-a") in
  let s = step s ~actor:a (Decide { decision = True_positive; reason = "passport matches" }) in
  let s = step s ~actor:b (Reject "check the second passport") in
  print_s [%sexp ((fst s).status : Case.Status.t)];
  ignore (step s ~actor:System (Assign "analyst-c") : _ * _);
  [%expect {|
    (2 (Escalated "same name, same date of birth"))
    (3 (Assigned analyst-a))
    (4 (Decided (decision True_positive) (reason "passport matches")))
    (5 Closed)
    (Closed (decision True_positive) (reason "passport matches")
     (decided_by analyst-a) (approved_by ()))
    (2 (Assigned analyst-a))
    (3 (Decided (decision True_positive) (reason "passport matches")))
    (4 Submitted_for_second_review)
    (5 (Rejected "check the second passport"))
    Open
    (refused
     ("only analysts act on cases" (case case-3) (t.status Open)
      (command (Assign analyst-c)) (actor System)))
    |}]
;;

let%expect_test "no case for a clear screening" =
  print_s
    [%sexp
      (Case.open_ ~id:"x" ~at:t0 (screen (person "Maria Garcia Lopez"))
       |> Or_error.map ~f:fst
       : Case.t Or_error.t)];
  [%expect {|
    (Error
     ("no case for a clear screening"
      (screening cde3fc4c238cc039b09d3f968c15ae63)))
    |}]
;;

(* Random command streams from two analysts: the replayed audit trail always gives the
   same case, and a discounted hit always has two different analysts. *)
let%expect_test "replay and four eyes hold for random command streams" =
  let gen =
    Quickcheck.Generator.(
      list_with_length
        12
        (tuple2
           (of_list [ a; b ])
           (union
              [ map (of_list [ "analyst-a"; "analyst-b" ]) ~f:(fun x -> Case.Command.Assign x)
              ; map Case.Analyst_decision.quickcheck_generator ~f:(fun decision ->
                  Case.Command.Decide { decision; reason = "r" })
              ; return Case.Command.Approve
              ; return (Case.Command.Reject "r")
              ; return (Case.Command.Escalate "e")
              ])))
  in
  let closed = ref 0 in
  Quickcheck.test gen ~sexp_of:[%sexp_of: (Case.Actor.t * Case.Command.t) list] ~f:(fun cmds ->
    let case, events = Case.open_ ~id:"q" ~at:t0 (force result) |> ok_exn in
    let case, log =
      List.fold cmds ~init:(case, events) ~f:(fun (case, log) (actor, cmd) ->
        match Case.apply case ~actor ~at:t0 cmd with
        | Ok (case, evs) -> case, log @ evs
        | Error _ -> case, log)
    in
    [%test_result: Case.t] (Case.of_events log |> ok_exn) ~expect:case;
    match case.status with
    | Closed { decision = False_positive; decided_by; approved_by; _ } ->
      incr closed;
      assert (
        match approved_by with
        | Some x -> not (String.equal x decided_by)
        | None -> false)
    | _ -> ());
  print_s [%sexp (!closed > 0 : bool)];
  [%expect {| true |}]
;;

let%expect_test "stable digest" =
  print_endline [%bin_digest: Case.Event.Stable.V1.t];
  print_endline [%bin_digest: Screening.Result.Stable.V1.t];
  [%expect {|
    510db407c853232d68afe345ed9b039e
    fee877c819330486782f75cc2da84519
    |}]
;;
