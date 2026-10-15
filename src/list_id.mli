(** Which published list an entry comes from. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Ofac_sdn (** US Treasury OFAC Specially Designated Nationals list *)
      | Eu_consolidated (** EU consolidated financial sanctions list (FSF) *)
      | Uk_ofsi (** UK OFSI consolidated list (closed on 28 January 2026) *)
      | Uk_sanctions_list (** UK Sanctions List, the sole UK source since then *)
      | Un_consolidated (** UN Security Council consolidated list *)
      | Other of string (** internal, PEP or test lists, by name *)
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Ofac_sdn
  | Eu_consolidated
  | Uk_ofsi
  | Uk_sanctions_list
  | Un_consolidated
  | Other of string
[@@deriving compare, equal, hash, sexp, quickcheck]

include Comparable.S with type t := t

(** ["ofac-sdn"], ["eu-consolidated"], ["uk-ofsi"], ["uk-sanctions-list"],
    ["un-consolidated"], or the name of an [Other] list. *)
val to_string : t -> string

val of_string : string -> t

(** The kyc-core source for a hit on this list ([Other] lists map to [Pep_list]). *)
val to_kyc : t -> Kyc_core.Screening.List_source.t
