(** EU consolidated financial sanctions list, XML (FSF format 1.1, namespace
    [http://eu.europa.ec/fpi/fsd/export]).

    One entry per [sanctionEntity] ([logicalId] is the id): [subjectType]
    [classificationCode] P (person) or E (entity); every [nameAlias] [wholeName] (the
    first is primary, [strong="false"] is a weak alias); [citizenship]
    [countryIso2Code]; [birthdate] (the [birthdate] attribute, else [year], a
    [circa="true"] year as one year either side); [regulation] [programme] and the
    earliest [publicationDate] as [listed_on]. The version is the date of
    [generationDate]. *)

open! Core

val parse : string -> Snapshot.t Or_error.t
