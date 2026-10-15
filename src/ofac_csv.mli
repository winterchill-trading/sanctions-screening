(** OFAC SDN list in the legacy CSV files: [SDN.CSV] (one row per party: ent_num, name,
    type, programmes, title, vessel fields, remarks) and [ALT.CSV] (aliases: ent_num,
    alt_num, type, name, remarks). [-0-] marks an empty field.

    Dates of birth, nationality and citizenship come from the remarks
    (["DOB 12 Mar 1961; alt. DOB 1962; nationality Russia; citizen Belarus"]), weak
    aliases from quoted remarks (["a.k.a. 'ABU FATHI'"]). Person names published as
    ["SURNAME, Given"] are stored as ["Given SURNAME"]. *)

open! Core

val parse : version:string -> sdn:string -> ?alt:string -> unit -> Snapshot.t Or_error.t

(** One remark date: ["12 Mar 1961"], ["Mar 1961"], ["1961"], ["circa 1968"],
    ["1959 to 1961"]. Exposed for tests. *)
val parse_dob : string -> Dob.t option
