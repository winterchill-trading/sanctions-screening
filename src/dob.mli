(** Dates of birth as lists publish them: a day, a month, a year, or a range of years
    ("circa 1960" is published as a range by some lists and kept as one here). *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Day of Date.Stable.V1.t
      | Month of
          { year : int
          ; month : int
          }
      | Year of int
      | Years of
          { from : int
          ; until : int
          }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Day of Date.t
  | Month of
      { year : int
      ; month : int
      }
  | Year of int
  | Years of
      { from : int
      ; until : int
      }
[@@deriving compare, equal, hash, sexp]

(** ["1960-03-12"], ["1960-03"], ["1960"], ["1959..1961"]; {!of_string} reads the same,
    and also ["circa 1960"] (one year either side). *)
val to_string : t -> string

val of_string : string -> t Or_error.t

(** How a subject's date of birth relates to an entry's. *)
module Agreement : sig
  type t =
    | Exact (** same day *)
    | Same_month
    | Same_year (** same year, or the year falls in the entry's range *)
    | Within_years of int (** this many years apart, within the tolerance *)
    | Mismatch
  [@@deriving sexp, compare, equal, bin_io]
end

(** Best agreement between a subject's date and any of an entry's dates; [None] when the
    entry has no date. *)
val agreement : tolerance_years:int -> t -> t list -> Agreement.t option
