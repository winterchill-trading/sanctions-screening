open! Core

module Analyst_decision = struct
  type t =
    | True_positive
    | False_positive
  [@@deriving bin_io, compare, equal, hash, sexp, enumerate, quickcheck]
end

module Four_eyes = struct
  type t =
    | Off
    | False_positive_only
    | All_decisions
  [@@deriving bin_io, compare, equal, hash, sexp, enumerate, quickcheck]

  let required t (d : Analyst_decision.t) =
    match t, d with
    | Off, _ -> false
    | All_decisions, _ -> true
    | False_positive_only, False_positive -> true
    | False_positive_only, True_positive -> false
  ;;
end

module Actor = struct
  type t =
    | Analyst of string
    | System
  [@@deriving bin_io, compare, equal, hash, sexp, quickcheck]
end

module Status = struct
  type t =
    | Open
    | Under_review of { analyst : string }
    | Awaiting_second_review of
        { decision : Analyst_decision.t
        ; reason : string
        ; first : string
        }
    | Escalated of { reason : string }
    | Closed of
        { decision : Analyst_decision.t
        ; reason : string
        ; decided_by : string
        ; approved_by : string option
        }
  [@@deriving bin_io, compare, equal, sexp]

  let is_final = function
    | Closed _ -> true
    | Open | Under_review _ | Awaiting_second_review _ | Escalated _ -> false
  ;;
end

module Command = struct
  type t =
    | Assign of string
    | Decide of
        { decision : Analyst_decision.t
        ; reason : string
        }
    | Approve
    | Reject of string
    | Escalate of string
    | Note of string
  [@@deriving sexp, compare, equal, quickcheck]
end

module Event = struct
  module Kind = struct
    type t =
      | Opened of
          { subject_id : string
          ; screening_id : string
          ; hits : string list
          ; four_eyes : Four_eyes.t
          }
      | Assigned of string
      | Decided of
          { decision : Analyst_decision.t
          ; reason : string
          }
      | Submitted_for_second_review
      | Approved
      | Rejected of string
      | Escalated of string
      | Noted of string
      | Closed
    [@@deriving bin_io, compare, equal, sexp]
  end

  module Stable = struct
    module V1 = struct
      type t =
        { case_id : string
        ; seq : int
        ; at : Time_ns.Stable.Alternate_sexp.V1.t
        ; actor : Actor.t
        ; kind : Kind.t
        }
      [@@deriving bin_io, compare, equal, sexp]
    end
  end

  type t = Stable.V1.t =
    { case_id : string
    ; seq : int
    ; at : Time_ns.Alternate_sexp.t
    ; actor : Actor.t
    ; kind : Kind.t
    }
  [@@deriving compare, equal, sexp]
end

type t =
  { id : string
  ; subject_id : string
  ; screening_id : string
  ; hits : string list
  ; four_eyes : Four_eyes.t
  ; status : Status.t
  ; version : int
  }
[@@deriving compare, equal, sexp_of]

(* The state change of one event. [Decided] alone does not close a case: a [Closed] or
   [Submitted_for_second_review] event follows it in the same batch. *)
let evolve t (e : Event.t) =
  let open Or_error.Let_syntax in
  let%map status =
    match e.kind, t.status, e.actor with
    | Opened _, _, _ -> Or_error.error_s [%message "case opened twice" t.id]
    | Assigned analyst, _, _ -> Ok (Status.Under_review { analyst })
    | Decided _, Under_review _, _ -> Ok t.status
    | Submitted_for_second_review, _, _ -> Ok t.status
    | Approved, Awaiting_second_review _, _ -> Ok t.status
    | Rejected _, Awaiting_second_review _, _ -> Ok Status.Open
    | Escalated reason, _, _ -> Ok (Status.Escalated { reason })
    | Noted _, s, _ -> Ok s
    | Closed, _, _ -> Ok t.status
    | (Decided _ | Approved | Rejected _), s, _ ->
      Or_error.error_s [%message "event does not apply" (e : Event.t) (s : Status.t)]
  in
  { t with status; version = e.seq }
;;

(* Status after a batch: the pending decision is carried in the events, so compute it from
   the batch rather than from single events. *)
let finish_batch t (events : Event.t list) =
  let decided =
    List.find_map events ~f:(fun e ->
      match e.kind, e.actor with
      | Decided { decision; reason }, Analyst a -> Some (decision, reason, a)
      | _ -> None)
  in
  let submitted =
    List.exists events ~f:(fun e ->
      [%equal: Event.Kind.t] e.kind Submitted_for_second_review)
  in
  let approved =
    List.find_map events ~f:(fun e ->
      match e.kind, e.actor with
      | Approved, Analyst b -> Some b
      | _ -> None)
  in
  let closed = List.exists events ~f:(fun e -> [%equal: Event.Kind.t] e.kind Closed) in
  let status : Status.t =
    match decided, submitted, approved, closed, t.status with
    | Some (decision, reason, first), true, _, _, _ ->
      Awaiting_second_review { decision; reason; first }
    | Some (decision, reason, a), false, _, true, _ ->
      Closed { decision; reason; decided_by = a; approved_by = None }
    | None, _, Some b, true, Awaiting_second_review { decision; reason; first } ->
      Closed { decision; reason; decided_by = first; approved_by = Some b }
    | _ -> t.status
  in
  { t with status }
;;

let apply_events t events =
  let open Or_error.Let_syntax in
  let%map t = List.fold_result events ~init:t ~f:evolve in
  finish_batch t events
;;

let open_ ~id ?(four_eyes = Four_eyes.False_positive_only) ~at (r : Screening.Result.t) =
  match r.decision with
  | Clear -> Or_error.error_s [%message "no case for a clear screening" ~screening:r.id]
  | Potential_match hits | Confirmed hits ->
    let hits = List.map hits ~f:Hit.entry_key in
    let event =
      { Event.case_id = id
      ; seq = 1
      ; at
      ; actor = System
      ; kind = Opened { subject_id = r.subject.id; screening_id = r.id; hits; four_eyes }
      }
    in
    Ok
      ( { id
        ; subject_id = r.subject.id
        ; screening_id = r.id
        ; hits
        ; four_eyes
        ; status = Open
        ; version = 1
        }
      , [ event ] )
;;

let apply t ~(actor : Actor.t) ~at (command : Command.t) =
  let open Or_error.Let_syntax in
  let refuse msg =
    Or_error.error_s
      [%message
        msg ~case:t.id (t.status : Status.t) (command : Command.t) (actor : Actor.t)]
  in
  let%bind analyst =
    match actor with
    | Analyst a -> Ok a
    | System -> refuse "only analysts act on cases"
  in
  let%bind kinds =
    match command, t.status with
    | Note text, _ -> Ok [ Event.Kind.Noted text ]
    | _, Closed _ -> refuse "case is closed"
    | Assign a, (Open | Escalated _ | Under_review _) -> Ok [ Event.Kind.Assigned a ]
    | Decide { decision; reason }, Under_review { analyst = assigned } ->
      if not (String.equal assigned analyst)
      then refuse "only the assigned analyst decides"
      else if String.is_empty (String.strip reason)
      then refuse "a decision needs a reason"
      else if Four_eyes.required t.four_eyes decision
      then Ok [ Event.Kind.Decided { decision; reason }; Submitted_for_second_review ]
      else Ok [ Decided { decision; reason }; Closed ]
    | (Approve | Reject _), Awaiting_second_review { first; _ } ->
      if String.equal first analyst
      then refuse "the second review needs a different analyst"
      else (
        match command with
        | Approve -> Ok [ Event.Kind.Approved; Closed ]
        | Reject reason -> Ok [ Rejected reason ]
        | _ -> assert false)
    | Escalate reason, (Open | Under_review _) -> Ok [ Event.Kind.Escalated reason ]
    | _ -> refuse "command not allowed in this state"
  in
  let events =
    List.mapi kinds ~f:(fun i kind ->
      { Event.case_id = t.id; seq = t.version + i + 1; at; actor; kind })
  in
  let%map t = apply_events t events in
  t, events
;;

let of_events events =
  let open Or_error.Let_syntax in
  match events with
  | [] -> Or_error.error_s [%message "no events"]
  | ({ Event.kind = Opened { subject_id; screening_id; hits; four_eyes }; _ } as first)
    :: rest ->
    let t =
      { id = first.case_id
      ; subject_id
      ; screening_id
      ; hits
      ; four_eyes
      ; status = Open
      ; version = first.seq
      }
    in
    let%bind () =
      List.foldi events ~init:(Ok ()) ~f:(fun i acc (e : Event.t) ->
        let%bind () = acc in
        if not (String.equal e.case_id t.id)
        then Or_error.error_s [%message "event of another case" (e : Event.t)]
        else if e.seq <> i + 1
        then
          Or_error.error_s [%message "sequence gap" ~expected:(i + 1 : int) (e : Event.t)]
        else Ok ())
    in
    (* Events are grouped into the batches [apply] produced: a batch starts with anything
       but [Submitted_for_second_review] or [Closed]. *)
    let batches =
      List.group rest ~break:(fun _ (b : Event.t) ->
        match b.kind with
        | Submitted_for_second_review | Closed -> false
        | _ -> true)
    in
    List.fold_result batches ~init:t ~f:apply_events
  | e :: _ -> Or_error.error_s [%message "first event is not Opened" (e : Event.t)]
;;
