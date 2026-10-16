(** One list entry that matched a screened subject, with everything an analyst (or an
    auditor) needs to see why. *)

open! Core

module Reason : sig
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
    | Weak_alias (** the matched name is a low-quality alias on the list *)
    | Visual_reading (** matched only after reading look-alike letters by shape *)
  [@@deriving sexp, compare, equal, bin_io]
end

module Priority : sig
  type t =
    | High
    | Medium
    | Low (** a strong name whose secondary attributes disagree *)
  [@@deriving sexp, compare, equal, bin_io]
end

module Stable : sig
  module V1 : sig
    type t =
      { list : List_id.Stable.V1.t
      ; entry_id : string
      ; list_version : string
      ; party : Party_type.Stable.V1.t
      ; matched_name : string (** the list's spelling *)
      ; subject_name : string (** the subject's spelling that matched *)
      ; name : Match.Name_score.t
      ; reasons : Reason.t list
      ; adjustment : int (** sum of the secondary adjustments *)
      ; score : int (** name score plus adjustment, clamped to 0 to 1000 *)
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

(** ["<list>:<entry_id>"], e.g. ["ofac-sdn:9001"]: unique across lists, where entry ids
    alone are not (two lists may both have an entry ["101"]). *)
val entry_key : t -> string

(** The list and the list's own entry id of an {!entry_key} (split at the first [':'];
    list names never contain one), e.g. to read back a kyc-core hit's [entry_id]. *)
val split_entry_key : string -> (List_id.t * string) option

(** One line for logs and case screens:
    ["ofac-sdn:9001 931 High jaro_winkler 'Oleg PARAMONOV' ~ 'Oleg Paramonov' dob=Exact"]. *)
val to_string_hum : t -> string

(** The hit as kyc-core records it: [entry_id] is the list-qualified {!entry_key}
    (["eu-consolidated:101"]), score scaled to 0 to 100, algorithm
    ["sanctions-screening/<best component>"]. The list's own id stays in [entry_id];
    {!split_entry_key} recovers it from the kyc-core record. *)
val to_kyc : t -> Kyc_core.Screening.Hit.t
