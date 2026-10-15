(** RFC 4180 CSV reading: quoted fields with doubled quotes, embedded separators and line
    breaks, CRLF or LF line ends, a leading byte order mark and a trailing DOS end-of-file
    byte (OFAC's files end with one) are accepted. *)

open! Core

(** All rows; an empty line is a row with one empty field and is skipped. *)
val parse : ?sep:char -> string -> string list list
