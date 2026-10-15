# Changes

## Unreleased

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
