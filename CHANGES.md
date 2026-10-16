# Changes

## Unreleased

## 0.1.1

- `Store_pg` (sanctions-screening-pg) owns one schema, `sanctions_screening` by default
  (`connect ~schema`): it creates it and keeps its tables and its own
  `schema_migrations` table inside it, so the host application's migrations table and
  other libraries no longer clash with it (all were at version 1 in one shared table).
  A 0.1.0 database is adopted on connect: the tables move into the schema and migration
  1 is recorded there. Breaking for SQL that reads the tables unqualified from `public`.
- `Hit.to_kyc` sets kyc-core's `entry_id` to the list-qualified `Hit.entry_key`
  (`eu-consolidated:7001`): entry ids repeat across lists and kyc-core refused a
  screening with two hits of the same id. `Hit.split_entry_key` reads it back; the raw
  id stays in `Hit.t.entry_id`. Breaking for code that matched kyc hits by raw id.
- `List_source.load_fixtures ()` returns the bundled fixture lists (embedded at build
  time, no files needed); `List_source.load_files` loads lists held in memory as (path,
  contents) pairs. The package installs the fixtures under
  `share/sanctions-screening/fixtures/`.
- UK Sanctions List XML parser (`Uk_xml`, format `Uk_sanctions_list_xml`), the FCDO
  list that replaced the OFSI consolidated list; `scripts/fetch-lists.sh` fetches it by
  default and the OFSI archive only with `--uk-ofsi`.
- Peak memory of the OFAC advanced XML load from 883 MB to 158 MB (CSV: 114 MB): the
  file is streamed from disk (`Xml.iter_elements_channel`, `Ofac_xml.of_channel`),
  parties are reduced to the fields used while reading, ids are interned, and other files
  are read in one allocation.

## 0.1.0

- Unicode fold (NFKD, marks, invisible code points, lowercase), Cyrillic, Greek and
  Arabic Latinisation (ICAO 9303, ELOT 743, BGN/PCGN), look-alike table, name
  normalisation with honorific, legal-form and vessel-prefix stripping, roman numerals.
- `Match`: Jaro-Winkler, Damerau-Levenshtein, token alignment with initials and
  patronymics, Soundex, consonant skeletons; integer per-mille scores; the primitives and
  `Match.score` are `[@zero_alloc]`.
- List parsers: OFAC SDN CSV and advanced XML, EU consolidated XML, UK OFSI CSV, an
  s-expression format for internal lists; `List_source.load_dir`; snapshots with digests
  and diffs.
- `Index` (trigram inverted index with Dice prefilter), `Lists`, `Policy`, `Subject`,
  `Hit`, `Decision`, `Screening.screen` and `Screening.rescreen`.
- `Case` lifecycle with optional four eyes and an event trail.
- `Store.S`, `Store_memory`, `Store_conformance`; package `sanctions-screening-pg` with
  `Store_pg` and migrations.
- `sanctions-screening` command (`screen`, `rescreen`, `stats`), `scripts/fetch-lists.sh`,
  benchmark, precision and recall harness.
- Security fix (screening evasion), found in review of the first normaliser: zero-width
  characters, soft hyphens and other invisible code points became word separators, letters
  of scripts without a table between U+0080 and U+2E7F were deleted, compatibility forms
  outside the BMP (mathematical letters) were not folded, and Cyrillic or Greek
  look-alikes inside Latin words were transliterated by sound (`р` as `r`). Invisible code
  points are now removed, unmapped letters kept, the decomposition table covers the
  supplementary planes, and disguised words get a second, visual reading that screening
  also matches. `test/adversarial.ml` pins the disguises that must still hit.
