open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Ofac_sdn
      | Eu_consolidated
      | Uk_ofsi
      | Uk_sanctions_list
      | Un_consolidated
      | Other of string
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

module T = struct
  type t = Stable.V1.t =
    | Ofac_sdn
    | Eu_consolidated
    | Uk_ofsi
    | Uk_sanctions_list
    | Un_consolidated
    | Other of string
  [@@deriving compare, equal, hash, sexp, quickcheck]
end

include T
include Comparable.Make (T)

let to_string = function
  | Ofac_sdn -> "ofac-sdn"
  | Eu_consolidated -> "eu-consolidated"
  | Uk_ofsi -> "uk-ofsi"
  | Uk_sanctions_list -> "uk-sanctions-list"
  | Un_consolidated -> "un-consolidated"
  | Other s -> s
;;

let of_string = function
  | "ofac-sdn" -> Ofac_sdn
  | "eu-consolidated" -> Eu_consolidated
  | "uk-ofsi" -> Uk_ofsi
  | "uk-sanctions-list" -> Uk_sanctions_list
  | "un-consolidated" -> Un_consolidated
  | s -> Other s
;;

let to_kyc : t -> Kyc_core.Screening.List_source.t = function
  | Ofac_sdn -> Ofac_sdn
  | Eu_consolidated -> Eu_fsf
  | Uk_ofsi | Uk_sanctions_list -> Uk_sanctions_list
  | Un_consolidated -> Un_sc
  | Other _ -> Pep_list
;;
