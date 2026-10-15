(** A screening alert's case: opened on a potential match, reviewed by analysts, closed as
    a true or false positive. A pure state machine: every command returns the new record
    and the events to append; {!of_events} rebuilds the record from the events, so the
    audit trail is the source of truth.

    {v
      open -> Open
      Open | Escalated | Under_review --- Assign a ---------> Under_review a
      Under_review a --- Decide by a --+-- no second review --> Closed
                                      +-- four eyes -------> Awaiting_second_review
      Awaiting_second_review (by a) --- Approve by b <> a ---> Closed
      Awaiting_second_review (by a) --- Reject by b <> a ----> Open
      Open | Under_review --- Escalate ---------------------> Escalated
      any state --- Note -----------------------------------> same state
    v}

    By default discounting a hit (false positive) needs a second analyst, as banking
    practice expects; confirming a true positive does not. Cases are internal: nothing
    here is meant for the customer (see {!Decision.customer_view}). *)

open! Core

module Analyst_decision : sig
  type t =
    | True_positive (** it is the listed party: freeze, reject, report *)
    | False_positive (** not the listed party: discount the hit *)
  [@@deriving bin_io, compare, equal, hash, sexp, enumerate, quickcheck]
end

module Four_eyes : sig
  type t =
    | Off
    | False_positive_only
    | All_decisions
  [@@deriving bin_io, compare, equal, hash, sexp, enumerate, quickcheck]

  val required : t -> Analyst_decision.t -> bool
end

module Actor : sig
  type t =
    | Analyst of string
    | System
  [@@deriving bin_io, compare, equal, hash, sexp, quickcheck]
end

module Status : sig
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

  val is_final : t -> bool
end

module Command : sig
  type t =
    | Assign of string (** analyst id *)
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

module Event : sig
  module Kind : sig
    type t =
      | Opened of
          { subject_id : string
          ; screening_id : string
          ; hits : string list (** entry keys *)
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

  module Stable : sig
    module V1 : sig
      type t =
        { case_id : string
        ; seq : int (** 1, 2, ... per case *)
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

type t = private
  { id : string
  ; subject_id : string
  ; screening_id : string
  ; hits : string list
  ; four_eyes : Four_eyes.t
  ; status : Status.t
  ; version : int (** seq of the last event *)
  }
[@@deriving compare, equal, sexp_of]

(** Opens a case for a screening that is not [Clear]. *)
val open_
  :  id:string
  -> ?four_eyes:Four_eyes.t
  -> at:Time_ns.t
  -> Screening.Result.t
  -> (t * Event.t list) Or_error.t

(** Validates the command against the state and the actor (an analyst; a second review
    must come from a different analyst), and returns the new state with its events. *)
val apply
  :  t
  -> actor:Actor.t
  -> at:Time_ns.t
  -> Command.t
  -> (t * Event.t list) Or_error.t

(** Rebuilds a case from its events, checking ids and sequence numbers. *)
val of_events : Event.t list -> t Or_error.t
