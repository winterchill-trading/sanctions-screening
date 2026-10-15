(** Name normalisation: the same pipeline for list entries and for screened names, so
    both sides compare in one alphabet.

    + {!Unicode_fold.fold}: NFKD, drop combining marks, lowercase.
    + {!Translit.latin}: Cyrillic, Greek and Arabic to Latin; the Greek digraph ου is
      written ou (ELOT 743).
    + Apostrophes are removed (["O'Neil"] becomes ["oneil"]); every other character that
      is not a letter or digit separates tokens.
    + Honorifics are dropped from person names, legal forms and joining words from entity
      names, vessel prefixes from vessel names; all of them when the type is unknown. A
      name made only of such words keeps them.
    + {!key} sorts the tokens so word order does not matter. *)

open! Core

(** Folded, Latinised, lowercase text with single spaces, in the original word order. *)
val fold : string -> string

(** Words dropped from person names: ["mr"; "dr"; "sheikh"; ...]. *)
val honorifics : string list

(** Words dropped from entity names: ["ltd"; "gmbh"; "ooo"; ...]. *)
val legal_forms : string list

(** Joining words dropped from entity names: ["the"; "and"]. *)
val joining_words : string list

(** Words dropped from vessel names: ["mv"; "mt"; ...]. *)
val vessel_prefixes : string list

(** {!fold}, split, and drop the words for the party type (default [Unknown]). *)
val tokens : ?party:Party_type.t -> string -> string list

module Key : sig
  (** A normalised name ready for matching. *)
  type t = private
    { tokens : string array (** sorted, duplicates removed *)
    ; sorted : string (** [tokens] joined by single spaces *)
    ; skeleton : string
    (** each token without vowels and doubled letters, sorted and joined: the form
        Arabic-script names reach after Latinisation *)
    ; abjad : bool (** the input contained Arabic-script letters *)
    }
  [@@deriving sexp_of, compare, equal]

  val is_empty : t -> bool
end

val key : ?party:Party_type.t -> string -> Key.t

(** Consonant skeleton of one ASCII token: ["muhammad"] is ["mhmd"]. *)
val skeleton_of_token : string -> string
