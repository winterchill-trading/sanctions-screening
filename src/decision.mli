(** The outcome of one screening. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Clear
      | Potential_match of Hit.Stable.V1.t list
      (** best first; an analyst decides (see {!Case}) *)
      | Confirmed of Hit.Stable.V1.t list
      (** the policy's confirm rule held for the first hit; the rest are kept *)
    [@@deriving bin_io, compare, equal, sexp]
  end
end

type t = Stable.V1.t =
  | Clear
  | Potential_match of Hit.t list
  | Confirmed of Hit.t list
[@@deriving compare, equal, sexp]

val hits : t -> Hit.t list

(** What the customer may be told. Alerts and cases must never be disclosed (tipping-off),
    so anything but [Clear] reads as a neutral review. *)
module Customer_view : sig
  type t =
    | No_action_needed
    | Under_review
  [@@deriving sexp, compare, equal]
end

val customer_view : t -> Customer_view.t

(** One line per hit, or ["clear"]. *)
val to_string_hum : t -> string
