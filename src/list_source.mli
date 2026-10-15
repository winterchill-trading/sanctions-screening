(** Reading published lists into {!Snapshot.t}s.

    Parsers, one per format:

    - {!Ofac_csv}: OFAC SDN legacy CSV ([SDN.CSV] and [ALT.CSV]).
    - {!Ofac_xml}: OFAC SDN advanced XML ([SDN_ADVANCED.XML]).
    - {!Eu_xml}: EU consolidated list XML (FSF 1.1).
    - {!Uk_csv}: UK consolidated list CSV as OFSI published it.
    - {!Fixture}: lists kept as s-expressions (internal, PEP, tests).

    Nothing here downloads; [scripts/fetch-lists.sh] fetches the public files. *)

open! Core

module Format : sig
  type t =
    | Ofac_sdn_csv
    | Ofac_advanced_xml
    | Eu_xml
    | Uk_ofsi_csv
    | Fixture
  [@@deriving sexp, compare, equal, enumerate]
end

(** Recognises a file by its name and first bytes: [*.sexp] is a fixture; XML in the OFAC
    advanced namespace or the EU export namespace; CSV starting ["Last Updated"] (UK); a
    file named [sdn.csv] (any case) is OFAC. [None] for anything else (including
    [alt.csv], read together with [sdn.csv]). *)
val detect : filename:string -> contents:string -> Format.t option

(** Parses one file. OFAC CSV takes the [alt.csv] contents as [?alt] and has no date
    inside, so [?version] names it (default ["unknown"]). *)
val parse : Format.t -> ?version:string -> ?alt:string -> string -> Snapshot.t Or_error.t

(** Every list file under [dir] (recursively), in path order. For OFAC CSV, [alt.csv] next
    to [sdn.csv] is read with it, and the version is the first line of a [VERSION] file in
    that directory, or the file's modification date. *)
val load_dir : string -> Snapshot.t list Or_error.t
