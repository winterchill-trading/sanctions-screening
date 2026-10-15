(** UK consolidated list as OFSI published it ([ConList.csv], "2022 format"): a
    ["Last Updated,dd/mm/yyyy"] line, a header, then one row per name.

    Rows are grouped by [Group ID]. The name is [Name 1] to [Name 5] then [Name 6] (the
    surname, or the whole name of an entity or ship). [Alias Type] "Primary name" is the
    primary name, "Primary name variation" and "AKA" are aliases ("Low quality" ones are
    weak), "FKA" is former; [Name Non-Latin Script] is an alias. [DOB] is dd/mm/yyyy with
    00 for unknown parts; [Nationality] may list several as ["(1) Russia (2) Belarus"].
    [Regime] gives the programmes and [Listed On] the listing date.

    OFSI closed this list on 28 January 2026; the UK Sanctions List replaced it. The parser
    reads archived files and any list kept in this format. *)

open! Core

val parse : string -> Snapshot.t Or_error.t

(** ["12/03/1961"], ["00/03/1961"], ["00/00/1961"]. *)
val parse_dob : string -> Dob.t option
