(** What a list entry or a screened subject is. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Person
      | Entity (** companies, organisations, groups *)
      | Vessel
      | Aircraft
      | Unknown
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Person
  | Entity (** companies, organisations, groups *)
  | Vessel
  | Aircraft
  | Unknown
[@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

(** Lowercase name, e.g. ["person"]. *)
val to_string : t -> string

val of_string : string -> t Or_error.t

(** [Unknown] is compatible with everything; otherwise equal types only. *)
val compatible : t -> t -> bool
