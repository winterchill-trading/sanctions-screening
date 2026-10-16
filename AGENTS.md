# sanctions-screening: agent guide

## Purpose

An OCaml library that screens names of customers and payment counterparties against
public sanctions lists (OFAC SDN, EU consolidated list, UK) and internal lists, and returns
explainable, deterministic decisions with integer scores. It also parses the list formats,
rescreens a population when a list changes, and runs a small case lifecycle.

## Layout

| Path | Holds |
| --- | --- |
| `src/` | Library `sanctions-screening` (module `Sanctions_screening`): Unicode fold, transliteration, normalisation, matching, parsers, index, screening, cases, `Store.S`, memory store, conformance suite |
| `pg/` | Library `sanctions-screening-pg`: `Store_pg` over `pg-async`, migrations embedded at build time |
| `migrations/` | PostgreSQL tables (`sanctions_` prefix), applied inside the library's own schema (default `sanctions_screening`) with its own `schema_migrations`; SQL lives only here and in `pg/store_pg.ml` |
| `bin/` | The `sanctions-screening` command: `screen`, `rescreen`, `stats` |
| `test/` | Expect tests, Quickcheck properties, adversarial spellings, precision and recall harness (`labelled.tsv`), cram test `cli.t` |
| `test-pg/` | The store conformance run on a throwaway PostgreSQL cluster |
| `bench/` | `core_bench` benchmark on a synthetic 20k-entry list (or real lists via `SANCTIONS_BENCH_LISTS`) |
| `fixtures/` | Sanitised list excerpts in each public format; every party is invented. Embedded in the library (`src/gen/gen_fixtures.ml`, `List_source.load_fixtures`) and installed under `share/` (`fixtures/dune`) |
| `scripts/` | `fetch-lists.sh` (real lists), table generators for Unicode data and country names |
| `.github/` | CI workflow |

## Build, test, docs

Run from this repository, inside the workspace:

    opam exec --switch 5.2.0+ox -- dune build --root .. @sanctions-screening/all
    opam exec --switch 5.2.0+ox -- dune build --root .. @sanctions-screening/runtest
    opam exec --switch 5.2.0+ox -- dune build --root .. @sanctions-screening/doc

Several agents building at once need `DUNE_BUILD_DIR=_build-sanctions-screening`
(resolved from the workspace root because of `--root ..`). The PostgreSQL test needs
`initdb` and `pg_ctl` (Homebrew `postgresql@18` or Debian `postgresql`). Benchmarks:
`dune build --root .. --profile release ./sanctions-screening/bench/bench.exe`, then run
the executable from the build directory.

Standalone: `opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`,
install the dependencies, then `dune build` here.

## Conventions

- Format this repository only: `dune build --root .. @sanctions-screening/fmt --auto-promote`.
  Never `dune fmt` across the workspace. `src/unicode_data.ml` and `src/country_names.ml`
  are generated (see `scripts/`) and listed in `.ocamlformat-ignore`.
- Every module has an `.mli` with odoc on every value; CI fails on any odoc warning, so
  write names from sibling packages as code spans, not `{!...}` references.
- Persisted types have `Stable.V1` modules; their `bin_digest`s are pinned in
  `test/test_entry.ml` and `test/test_case.ml`. A changed digest needs a new version.
- Scores are integers per mille. Never introduce floats into scoring.
- The comparison primitives and `Match.score` carry `[@zero_alloc]` in `match.mli`; the
  compiler checks them. Local closures break it (see `ends_with` in `match.ml`).
  `test/test_match.ml` pins 0 words per call; words per call are stable across
  platforms here because nothing on that path touches `Hashtbl` or JSON.
- Normalisation must never delete a letter or let an invisible code point split a word;
  add any new disguise to `test/adversarial.ml` (both tests read it).
- Changing weights or thresholds: rerun `test/test_precision_recall.ml` and update the
  README "Numbers" table; check the prefilter line in `test/test_screening.ml` stays at 0
  missed.
- Comments for humans at non-obvious choices; a `CHANGES.md` "Unreleased" entry for every
  user-visible change.

## Dependencies and dependents

- Siblings: `kyc-core` (hit and reason types), `pg-async` (only `pg/` and `test-pg/`).
- External: `core`, `core_unix`, `async_kernel`, `ppx_jane`; `async`, `core_bench` for
  tests and benchmarks.
- CI pin order: `kyc-core`, `pg-async`.
- Dependents: none in the organisation yet.

## CI

`.github/workflows/ci.yml` installs `libpq-dev` and `postgresql`, runs the infra
`ox-setup` action with the pins, `shellcheck scripts/*.sh`, `dune build`, `dune test`,
then the odoc build with warnings as errors; the `obj-magic` job calls the reusable
workflow in `winterchill-trading/infra`. To check a run: `gh run list --limit 1`, then at
most one `gh run watch <id> --exit-status`. Never poll in a loop.

## Pitfalls

- A `[@zero_alloc]` function cannot build a local closure or call through one; lift
  helpers to the top level.
- Inline tests run from `_build/.../test`, so fixtures are read as `../fixtures/...` and
  declared with `(source_tree ../fixtures)` in `test/dune`; `labelled.tsv` is a declared
  dep too.
- The list fixtures keep CRLF line ends like the published files; `.gitattributes` marks
  `fixtures/**` as binary-exact. Do not let an editor rewrite them.
- A new fixture file is embedded automatically but must also be listed in
  `fixtures/dune` to be installed; adding entries changes the prefilter count, result
  ids and the precision table (check the labelled negatives do not start hitting it).
- `pg/store_pg.ml` writes every table as `{s}.table` and `q` puts the schema in; never
  write an unqualified table name there, and never edit `migrations/0001_init.sql` (its
  checksum is how a 0.1.0 install is recognised and adopted).
- `Hit.to_kyc` uses the list-qualified key as kyc-core's `entry_id`; keep it, ids collide
  across lists.
- The OFAC XML is parsed from a channel; anything that keeps an `Xml.t` per party alive
  brings the 880 MB peak back. Measure with `/usr/bin/time -l sanctions-screening stats
  -lists <dir>` on fetched lists after touching `xml.ml` or `ofac_xml.ml`.
- An OFAC CSV and an OFAC XML in one directory duplicate every entry id, and
  `Lists.create` refuses that.
- Other agents' half-written repositories can print errors in a workspace build; filter
  the output for `sanctions-screening` paths.
- `Time_ns` sexps print in the machine's zone; stored types use
  `Time_ns.Stable.Alternate_sexp.V1`.

## Where to read more

- `README.md` (design, formats, scores, wiring, numbers, known gaps), `CHANGES.md`
- `../specs/repos/sanctions-screening.md`
- `../oxcaml-claude-skills/AGENTS.md` and `../oxcaml-claude-skills/GOTCHAS.md`
- Skills in `../oxcaml-claude-skills/skills/`: `banking-standards`, `oxcaml-performance`,
  `janestreet-ocaml-style`

## Never do

- Run `opam install`, `upgrade` or `pin` that would change `ocaml-variants`, `core`, `base`,
  `async` or `ppx_jane` on the shared `5.2.0+ox` switch.
- Publish, tag or release.
- Commit real list files or real people's data; tests read the invented fixtures only.
- Commit credentials or print secrets (the library needs none), or poll CI in a loop.
