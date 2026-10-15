(** Name normalisation: the same pipeline for list entries and for screened names, so
    both sides compare in one alphabet.

    + {!Unicode_fold.fold}: NFKD (so fullwidth and stylised letters become plain ones),
      drop combining marks and invisible code points (zero-width space and joiners, soft
      hyphen, bidi controls, variation selectors), lowercase.
    + {!Translit.latin}: Cyrillic, Greek and Arabic to Latin; the Greek digraph ου is
      written ou (ELOT 743).
    + Apostrophes are removed (["O'Neil"] becomes ["oneil"]); every other character that
      is not a letter or digit separates tokens. A letter or digit of a script without
      a table is kept as it is; nothing that is a letter is ever deleted.
    + Honorifics are dropped from person names, legal forms and joining words from entity
      names, vessel prefixes from vessel names; all of them when the type is unknown. A
      name made only of such words keeps them.
    + {!key} sorts the tokens so word order does not matter. *)

open! Core

(** How look-alike letters are read. *)
module Reading : sig
  type t =
    | Phonetic (** by transliteration: Cyrillic р is r *)
    | Visual
    (** by shape ({!Translit.confusables}): Cyrillic р is p, and digits inside words
        read as letters (0 o, 1 l, 3 e, 5 s) *)
  [@@deriving sexp_of, compare, equal]
end

(** Folded, Latinised, lowercase text with single spaces, in the original word order
    (the phonetic reading). *)
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
    ; compact : string
    (** the words in their original order with no separators, so ["I.va-nov"] and
        ["Ivanov"] agree *)
    ; skeleton : string
    (** each token without vowels and doubled letters, sorted and joined: the form
        Arabic-script names reach after Latinisation *)
    ; abjad : bool (** the input contained Arabic-script letters *)
    ; reading : Reading.t
    }
  [@@deriving sexp_of, compare, equal]

  val is_empty : t -> bool
end

(** The phonetic key. *)
val key : ?party:Party_type.t -> string -> Key.t

(** Every reading worth screening, the phonetic key first. A second, visual key is added
    when the name mixes Latin letters with Cyrillic, Greek or Armenian ones, when all of
    its non-Latin letters are Latin look-alikes, or when a word mixes letters and digits:
    the shapes a name takes when it is disguised. Screening matches each key and keeps
    the best score, so a disguise can only add hits. *)
val keys : ?party:Party_type.t -> string -> Key.t list

(** Consonant skeleton of one ASCII token: ["muhammad"] is ["mhmd"]. *)
val skeleton_of_token : string -> string
