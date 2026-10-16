(** OFAC SDN list in the advanced XML format ([SDN_ADVANCED.XML]): reference value sets,
    locations, distinct parties with identities, aliases and features, and sanctions
    entries with their programmes.

    Read in one streaming pass over the second level of the document, so the 130 MB file
    is never held as one tree; each party is reduced to the fields below as soon as it is
    read, with repeated ids shared. {!of_channel} also reads the file a chunk at a time,
    so it is never held as one string either (see the README for peak memory). What is
    used:

    - [DistinctParty/Profile]: the party type from [PartySubTypeID] through
      [PartySubTypeValues] and [PartyTypeValues] (Individual, Entity, Transport as Vessel
      or Aircraft).
    - [Alias/DocumentedName]: one name per documented name, in every script; parts typed
      as last names are put last. [Primary="true"] is the primary name, an F.K.A. alias is
      [Former], [LowQuality="true"] is [Weak_alias].
    - Features named [Birthdate] (from the [DatePeriod]), [Nationality Country] and
      [Citizenship Country] (through [Locations] to the country's ISO2 code).
    - [SanctionsEntry]: measures of type [Program] give the programmes, the first entry
      event's date gives [listed_on].
    - [DateOfIssue] gives the version (["2026-10-15"]). *)

open! Core

val parse : string -> Snapshot.t Or_error.t

(** {!parse} reading from a channel, a chunk at a time. *)
val of_channel : In_channel.t -> Snapshot.t Or_error.t
