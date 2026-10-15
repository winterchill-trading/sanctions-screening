(** Screening a subject against lists, and rescreening a population when a list changes.

    Deterministic: the same subject, lists, policy, context and time give the same result,
    so a decision can be replayed from its stored inputs. *)

open! Core

module Context : sig
  module Role : sig
    type t =
      | Originator (** the payer of an incoming payment *)
      | Beneficiary (** the payee of an outgoing payment *)
      | Payee (** a saved payee, screened when it is created or changed *)
    [@@deriving bin_io, compare, equal, hash, sexp]
  end

  module Stable : sig
    module V1 : sig
      type t =
        | Onboarding
        | Data_change
        | Periodic
        | List_update
        | Counterparty of
            { role : Role.t
            ; reference : string
            (** payment or payee id; SEPA end-to-end id for scheme payments *)
            }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Onboarding
    | Data_change
    | Periodic
    | List_update
    | Counterparty of
        { role : Role.t
        ; reference : string
        }
  [@@deriving compare, equal, hash, sexp]

  val to_kyc : t -> Kyc_core.Screening.Screen_reason.t
end

module Result : sig
  module Stable : sig
    module V1 : sig
      type t =
        { id : string (** derived from the inputs, see {!screen} *)
        ; subject : Subject.Stable.V1.t
        ; context : Context.Stable.V1.t
        ; decision : Decision.Stable.V1.t
        ; lists : List_version.Stable.V1.t list
        ; policy_digest : string
        ; at : Time_ns.Stable.Alternate_sexp.V1.t
        }
      [@@deriving bin_io, compare, equal, sexp]
    end
  end

  type t = Stable.V1.t =
    { id : string
    ; subject : Subject.t
    ; context : Context.t
    ; decision : Decision.t
    ; lists : List_version.t list
    ; policy_digest : string
    ; at : Time_ns.Alternate_sexp.t
    }
  [@@deriving compare, equal, sexp]

  (** Entry keys of the hits. *)
  val hit_keys : t -> String.Set.t
end

(** Screens every name of the subject, in every reading ({!Normalize.keys}), against the
    lists. Per entry the best-scoring name counts. Hits are sorted by score, then entry
    key, and cut to [policy.max_hits].

    [id] is the MD5 (hex) of the subject, context, list digests, policy digest and time,
    so a retried call with the same inputs gives the same id. [?scratch] avoids allocating
    comparison buffers per call. *)
val screen
  :  ?scratch:Match.Scratch.t
  -> policy:Policy.t
  -> lists:Lists.t
  -> context:Context.t
  -> at:Time_ns.t
  -> Subject.t
  -> Result.t

(** The outcome of rescreening a population after a list update. *)
module Rescreen : sig
  type change =
    { subject : Subject.t
    ; previous : Decision.t
    ; result : Result.t (** full screening against the new lists *)
    }
  [@@deriving sexp_of]

  type t =
    { diff : (List_id.t * Snapshot.Diff.t) list (** per list present in either side *)
    ; newly_hit : change list
    (** subjects with a hit on an entry they did not hit before *)
    ; cleared : change list (** subjects that had hits and are now clear *)
    ; rescreened : int (** subjects screened again in full *)
    ; unchanged : int (** subjects the list delta could not affect *)
    }
  [@@deriving sexp_of]
end

(** Only subjects the delta can affect are screened again in full: those with a hit on an
    added or changed entry (found by screening against an index of the delta alone), and
    those whose previous hits include a removed or changed entry. *)
val rescreen
  :  ?scratch:Match.Scratch.t
  -> policy:Policy.t
  -> before:Lists.t
  -> after:Lists.t
  -> at:Time_ns.t
  -> (Subject.t * Decision.t) list
  -> Rescreen.t Or_error.t
