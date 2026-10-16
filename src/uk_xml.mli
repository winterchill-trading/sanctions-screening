(** The UK Sanctions List in its XML format ([UK-Sanctions-List.xml], schema 4.33),
    published by the Foreign, Commonwealth and Development Office at
    [https://sanctionslist.fcdo.gov.uk/docs/UK-Sanctions-List.xml] (format guide: "Format
    guide for the UK Sanctions List" on GOV.UK). Since 28 January 2026 it is the only
    source of UK designations; it replaced the OFSI consolidated list ({!Uk_csv}).

    One [Designation] per listed party, read one at a time. What is used:

    - [UniqueID] (e.g. ["RUS0001"]) is the entry id; [IndividualEntityShip] the party type
      (Individual, Entity, Ship as a vessel).
    - [Names/Name]: [Name1] to [Name5] then [Name6] (the surname or the entity's name),
      joined by spaces. [NameType] "Primary Name" is primary; "Primary Name Variation" and
      "Alias" are aliases, and an alias with [AliasStrength] "Low quality a.k.a" is
      [Weak_alias]. Case and stray capitals in these values are ignored, as the published
      file mixes them.
    - [NonLatinNames/NonLatinName/NameNonLatinScript] as aliases.
    - [DOB] values: [dd/mm/yyyy], with [dd] and [mm] as written placeholders for unknown
      parts, or a bare year.
    - [Nationality] country names, to ISO codes; [RegimeName] as the programme;
      [DateDesignated] as [listed_on]; [DateGenerated] as the version.

    Addresses, documents, positions and ship identifiers are skipped. *)

open! Core

val parse : string -> Snapshot.t Or_error.t

(** {!parse} reading from a channel, a chunk at a time. *)
val of_channel : In_channel.t -> Snapshot.t Or_error.t
