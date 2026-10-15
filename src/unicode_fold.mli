(** Unicode folding for name matching: compatibility decomposition (NFKD), removal of
    combining marks and invisible (default-ignorable) code points, and simple lowercasing.

    Decompositions cover every assigned code point except Hangul syllables and CJK
    ideographs (see [scripts/gen-unicode-tables.py]); those pass through unchanged and are
    never dropped. *)

(** Unicode version of the tables, e.g. ["16.0.0"]. *)
val unicode_version : string

(** Calls [f] with each code point of the full compatibility decomposition of [cp] (just
    [cp] when it has none). *)
val decompose : int -> f:(int -> unit) -> unit

(** General category Mn, Mc or Me. *)
val is_mark : int -> bool [@@zero_alloc]

(** Default_Ignorable_Code_Point: zero-width space, joiners, soft hyphen, bidi controls,
    variation selectors, tags. Invisible, so a name could hide them between letters. *)
val is_ignorable : int -> bool
[@@zero_alloc]

(** General category starting with L (letter) or N (number). *)
val is_letter_or_digit : int -> bool
[@@zero_alloc]

(** Simple lowercase mapping; [cp] when it has none. *)
val lower : int -> int
[@@zero_alloc]

(** [fold s] is [s] decomposed (NFKD), without combining marks or default-ignorable code
    points, lowercased, as UTF-8: ["Ĳsselmeer Ångström"] folds to ["ijsselmeer angstrom"].
    Idempotent. *)
val fold : string -> string
