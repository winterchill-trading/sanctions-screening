open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Person
      | Entity
      | Vessel
      | Aircraft
      | Unknown
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Person
  | Entity
  | Vessel
  | Aircraft
  | Unknown
[@@deriving compare, equal, hash, sexp, quickcheck, enumerate]

let to_string = function
  | Person -> "person"
  | Entity -> "entity"
  | Vessel -> "vessel"
  | Aircraft -> "aircraft"
  | Unknown -> "unknown"
;;

let of_string s =
  match String.lowercase (String.strip s) with
  | "person" | "individual" -> Ok Person
  | "entity" | "enterprise" | "organisation" | "organization" -> Ok Entity
  | "vessel" | "ship" -> Ok Vessel
  | "aircraft" -> Ok Aircraft
  | "unknown" | "" -> Ok Unknown
  | other -> Or_error.error_s [%message "unknown party type" other]
;;

let compatible a b =
  match a, b with
  | Unknown, _ | _, Unknown -> true
  | a, b -> equal a b
;;
