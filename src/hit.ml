open! Core

module Reason = struct
  type t =
    | Dob of Dob.Agreement.t
    | Nationality_match of string
    | Nationality_mismatch of
        { subject : string list
        ; entry : string list
        }
    | Type_mismatch of
        { subject : Party_type.Stable.V1.t
        ; entry : Party_type.Stable.V1.t
        }
    | Weak_alias
    | Visual_reading
  [@@deriving sexp, compare, equal, bin_io]
end

module Priority = struct
  type t =
    | High
    | Medium
    | Low
  [@@deriving sexp, compare, equal, bin_io]
end

module Stable = struct
  module V1 = struct
    type t =
      { list : List_id.Stable.V1.t
      ; entry_id : string
      ; list_version : string
      ; party : Party_type.Stable.V1.t
      ; matched_name : string
      ; subject_name : string
      ; name : Match.Name_score.t
      ; reasons : Reason.t list
      ; adjustment : int
      ; score : int
      ; priority : Priority.t
      ; programmes : string list
      }
    [@@deriving bin_io, compare, equal, sexp]
  end
end

type t = Stable.V1.t =
  { list : List_id.t
  ; entry_id : string
  ; list_version : string
  ; party : Party_type.t
  ; matched_name : string
  ; subject_name : string
  ; name : Match.Name_score.t
  ; reasons : Reason.t list
  ; adjustment : int
  ; score : int
  ; priority : Priority.t
  ; programmes : string list
  }
[@@deriving compare, equal, sexp]

let entry_key t = List_id.to_string t.list ^ ":" ^ t.entry_id

let reason_to_string : Reason.t -> string = function
  | Dob a -> "dob=" ^ Sexp.to_string [%sexp (a : Dob.Agreement.t)]
  | Nationality_match c -> "nationality=" ^ c
  | Nationality_mismatch _ -> "nationality-mismatch"
  | Type_mismatch { subject; entry } ->
    sprintf "type=%s/%s" (Party_type.to_string subject) (Party_type.to_string entry)
  | Weak_alias -> "weak-alias"
  | Visual_reading -> "visual-reading"
;;

let to_string_hum t =
  String.concat
    ~sep:" "
    ([ entry_key t
     ; Int.to_string t.score
     ; Sexp.to_string [%sexp (t.priority : Priority.t)]
     ; t.name.best
     ; sprintf "'%s' ~ '%s'" t.matched_name t.subject_name
     ]
     @ List.map t.reasons ~f:reason_to_string)
;;

let to_kyc t : Kyc_core.Screening.Hit.t =
  { source = List_id.to_kyc t.list
  ; list_version = t.list_version
  ; entry_id = t.entry_id
  ; matched_name = t.matched_name
  ; score = (t.score + 5) / 10
  ; algorithm = "sanctions-screening/" ^ t.name.best
  }
;;
