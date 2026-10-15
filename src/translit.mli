(** Latinisation of folded (lowercase, mark-free) text.

    The tables follow public standards, simplified to ASCII:

    - Cyrillic: ICAO Doc 9303 (the machine-readable passport transliteration), with the
      Ukrainian, Belarusian, Serbian, Macedonian and common Central Asian letters.
    - Greek: ELOT 743 / ISO 843 letter by letter (no digraph rules).
    - Arabic and Persian: BGN/PCGN 1956 without diacritics; ain and hamza are dropped and
      short vowels, which the script does not write, stay missing. {!Match} makes up for
      the missing vowels with a consonant-skeleton comparison.
    - Latin letters with no decomposition: ß ss, æ ae, ø o, đ d, ł l, þ th, œ oe, ı i, ŋ
      ng, ð d.

    Input is expected to have gone through {!Unicode_fold.fold}, so accented and uppercase
    forms are already reduced to these base letters. *)

open! Core

(** Each table as (code point, Latin) pairs, for documentation and tests. *)
val cyrillic : (int * string) list

val greek : (int * string) list
val arabic : (int * string) list
val latin_extra : (int * string) list

(** The Latin spelling of one code point: [Some] for a letter or digit of a covered
    script, [None] otherwise (ASCII included). *)
val latin : int -> string option

(** Lowercase Cyrillic, Greek and Armenian letters that look like a Latin letter (after
    lowercasing), as (code point, Latin) pairs, after the Unicode confusables data:
    Cyrillic р looks like p (its transliteration is r), с like c, х like x, Greek ν like

    v. Used for the confusable reading of a name that mixes scripts. *)
val confusables : (int * string) list

(** The Latin look-alike of one code point, if it has one. *)
val confusable : int -> string option

(** Cyrillic, Greek or Armenian letter (U+0370 to U+058F). *)
val is_lookalike_script : int -> bool

(** Code point of an Arabic-script letter (U+0600 to U+06FF, and the presentation forms).
    Used to flag consonant-only spellings. *)
val is_abjad : int -> bool
