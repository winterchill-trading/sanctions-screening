open! Core

module Adjustments = struct
  type t =
    { dob_exact : int
    ; dob_same_month : int
    ; dob_same_year : int
    ; dob_within_tolerance : int
    ; dob_mismatch : int
    ; nationality_match : int
    ; nationality_mismatch : int
    ; type_mismatch : int
    ; weak_alias : int
    }
  [@@deriving sexp, compare, equal, bin_io]

  let default =
    { dob_exact = 60
    ; dob_same_month = 40
    ; dob_same_year = 25
    ; dob_within_tolerance = 5
    ; dob_mismatch = -120
    ; nationality_match = 20
    ; nationality_mismatch = -40
    ; type_mismatch = -150
    ; weak_alias = -50
    }
  ;;
end

module Confirm_rule = struct
  type t = { min_name_score : int } [@@deriving sexp, compare, equal, bin_io]
end

type t =
  { name_threshold : int
  ; weights : Match.Weights.t
  ; dob_tolerance_years : int
  ; adjustments : Adjustments.t
  ; secondary_can_clear : bool
  ; confirm : Confirm_rule.t option
  ; high_priority_at : int
  ; max_hits : int
  ; prefilter : int
  }
[@@deriving sexp, compare, equal, bin_io]

let default =
  { name_threshold = 850
  ; weights = Match.Weights.default
  ; dob_tolerance_years = 1
  ; adjustments = Adjustments.default
  ; secondary_can_clear = false
  ; confirm = None
  ; high_priority_at = 920
  ; max_hits = 20
  ; prefilter = 200
  }
;;

let payments = { default with name_threshold = 870 }
let digest t = Md5.to_hex (Md5.digest_string (Bin_prot.Writer.to_string bin_writer_t t))
