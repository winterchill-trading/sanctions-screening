(** Who is being screened: a customer at onboarding, a payee, a payment's originator or
    beneficiary. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      { id : string (** the caller's id: customer id, payee id, payment id plus role *)
      ; party : Party_type.Stable.V1.t
      ; names : string list (** legal name first, then other known names *)
      ; dob : Dob.Stable.V1.t option
      ; nationalities : string list (** ISO 3166 alpha-2 *)
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { id : string
  ; party : Party_type.t
  ; names : string list
  ; dob : Dob.t option
  ; nationalities : string list
  }
[@@deriving compare, equal, hash, sexp]

(** Fails without an id or a non-blank name; upper-cases nationalities. *)
val create
  :  id:string
  -> ?party:Party_type.t
  -> names:string list
  -> ?dob:Dob.t
  -> ?nationalities:string list
  -> unit
  -> t Or_error.t

(** A payment counterparty: only a name and, when known, a country (the account's or
    address country, used like a nationality). [party] defaults to [Unknown]. *)
val counterparty
  :  id:string
  -> ?party:Party_type.t
  -> name:string
  -> ?country:string
  -> unit
  -> t Or_error.t
