(** Screening thresholds and adjustments. All scores are per mille (0 to 1000).

    A list name becomes a hit when its {e name score} ({!Match.score}) reaches
    [name_threshold]. Secondary attributes then move the hit's {e total} up or down and
    set its priority; by default they never remove a hit (a strong name match with a
    different date of birth still goes to an analyst), because lists often carry wrong or
    partial dates. *)

open! Core

module Adjustments : sig
  type t =
    { dob_exact : int
    ; dob_same_month : int
    ; dob_same_year : int
    ; dob_within_tolerance : int
    ; dob_mismatch : int (** negative *)
    ; nationality_match : int
    ; nationality_mismatch : int (** negative *)
    ; type_mismatch : int (** negative: a person matched against a company *)
    ; weak_alias : int (** negative: the list itself calls the alias low quality *)
    }
  [@@deriving sexp, compare, equal, bin_io]

  val default : t
end

module Confirm_rule : sig
  (** When a screening may report [Confirmed] without an analyst: the name score is at
      least [min_name_score] and the date of birth agrees exactly. *)
  type t = { min_name_score : int } [@@deriving sexp, compare, equal, bin_io]
end

type t =
  { name_threshold : int
  ; weights : Match.Weights.t
  ; dob_tolerance_years : int
  ; adjustments : Adjustments.t
  ; secondary_can_clear : bool
  (** drop hits whose total falls below [name_threshold]; off by default *)
  ; confirm : Confirm_rule.t option
  ; high_priority_at : int (** totals at or above are [High] *)
  ; max_hits : int (** hits kept per screening, best first *)
  ; prefilter : int
  (** index candidate filter: Dice overlap of name trigrams, per mille; see {!Index} *)
  }
[@@deriving sexp, compare, equal, bin_io]

(** For onboarding: [name_threshold] 850, date tolerance 1 year, no auto-confirm,
    [high_priority_at] 920, 20 hits, prefilter 250. The values were chosen on the
    labelled set in [test/] (see the README for the numbers). *)
val default : t

(** For payment counterparties, where only a name (and sometimes a country) is known:
    {!default} with [name_threshold] 870, so that common partial names raise fewer
    alerts on high volumes. *)
val payments : t

(** MD5 of the bin_prot encoding, hex: stored with each result so a decision can be
    traced to the exact settings. *)
val digest : t -> string
