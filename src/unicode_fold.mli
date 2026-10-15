(** Unicode folding for name matching: compatibility decomposition (NFKD), removal of
    combining marks, and simple lowercasing.

    Covers the Basic Multilingual Plane except Hangul syllables and CJK ideographs (see
    [scripts/gen-unicode-tables.py]). Code points outside the tables pass through
    unchanged. *)

(** Unicode version of the tables, e.g. ["16.0.0"]. *)
val unicode_version : string

(** Calls [f] with each code point of the full compatibility decomposition of [cp] (just
    [cp] when it has none). *)
val decompose : int -> f:(int -> unit) -> unit

(** General category Mn, Mc or Me. *)
val is_mark : int -> bool [@@zero_alloc]

(** Simple lowercase mapping; [cp] when it has none. *)
val lower : int -> int [@@zero_alloc]

(** [fold s] is [s] decomposed (NFKD), without combining marks, lowercased, as UTF-8:
    ["Ĳsselmeer Ångström"] folds to ["ijsselmeer angstrom"]. Idempotent. *)
val fold : string -> string
