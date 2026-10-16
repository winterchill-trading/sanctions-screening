# sanctions-screening

Name screening against public sanctions lists for banks and payment firms: list parsers,
name normalisation, fuzzy matching, an in-memory index, explainable screening decisions,
rescreening on list updates and a small case lifecycle. Provider-neutral, deterministic,
integer scores only.

Two opam packages:

- `sanctions-screening`: the library (module `Sanctions_screening`) and the
  `sanctions-screening` command. Depends on `core`, `core_unix`, `async_kernel` and
  `kyc-core` (for the hit and reason types onboarding records).
- `sanctions-screening-pg`: `Store_pg`, the PostgreSQL store over `pg-async`.

## Why separate

Screening is needed by onboarding, payee management and payment processing alike, and
its rules (which list, which spelling, which threshold, who may discount a hit) change on
their own schedule. Keeping it out of the KYC and payment libraries lets one tuned and
tested matcher serve all of them, lets lists be refreshed without touching anything else,
and keeps the evidence an auditor asks for (list version, policy digest, per-component
scores) in one format.

## Quick start

```ocaml
open! Core
open Sanctions_screening

let lists = Lists.create (List_source.load_dir "lists" |> ok_exn) |> ok_exn

(* Or, with no files at all: the bundled invented fixture lists (tests, demos, offline
   mode), or lists already in memory. *)
let demo_lists = Lists.create (List_source.load_fixtures () |> ok_exn) |> ok_exn
let fetched_lists files = List_source.load_files files (* (path, contents) list *)

let decision =
  let subject =
    Subject.create
      ~id:"customer-42"
      ~party:Person
      ~names:[ "Oleg Paramonov" ]
      ~dob:(Day (Date.of_string "1961-03-12"))
      ~nationalities:[ "RU" ]
      ()
    |> ok_exn
  in
  (Screening.screen
     ~policy:Policy.default
     ~lists
     ~context:Onboarding
     ~at:(Time_ns.now ())
     subject)
    .decision
```

```sh
scripts/fetch-lists.sh lists/
sanctions-screening stats -lists lists/
sanctions-screening screen -name "Oleg Paramonov" -dob 1961-03-12 -lists lists/ -explain
sanctions-screening rescreen subjects.sexp -before lists-old/ -after lists/
```

## Lists and formats

| Format | Module | Files | What is read |
| --- | --- | --- | --- |
| OFAC SDN, legacy CSV | `Ofac_csv` | `sdn.csv`, `alt.csv`, optional `VERSION` | names (`SURNAME, Given` stored as `Given SURNAME`), aliases (aka, fka), weak aliases from quoted remarks, DOB/nationality/citizenship from remarks, programmes |
| OFAC SDN, advanced XML | `Ofac_xml` | `SDN_ADVANCED.XML` | party type, every documented name in every script (last names last), primary/aka/fka/low-quality, Birthdate and Nationality/Citizenship features through `Locations`, programmes and listing date from `SanctionsEntries`; version from `DateOfIssue` |
| EU consolidated list, XML (FSF 1.1) | `Eu_xml` | `*.xml` in the FSF namespace | `nameAlias` (first is primary, `strong="false"` weak), `citizenship`, `birthdate` (day, year, circa), `regulation` programme and date; version from `generationDate` |
| UK Sanctions List (FCDO), XML | `Uk_xml` | `UK-Sanctions-List.xml` (root `Designations`) | `UniqueID`, party type, `Name1` to `Name6` with name type (primary, variation, alias) and alias strength, non-Latin names, `DOB` with `dd`/`mm` placeholders or a bare year, nationality names, regime, `DateDesignated`; version from `DateGenerated` |
| UK consolidated list as OFSI published it, CSV | `Uk_csv` | `ConList.csv` | rows grouped by `Group ID`, Name 1 to 6, alias type and quality, non-Latin names, `dd/mm/yyyy` dates with `00` parts, numbered nationalities, regime, listing date |
| Internal and test lists | `Fixture` | `*.sexp` | the normalised entry, by hand |

`List_source.load_dir` detects files by name and first bytes and returns one `Snapshot.t`
per file (`List_source.load_files` does the same for (path, contents) pairs held in
memory, e.g. fetched over HTTP): a `List_version.t` (list, version, entry count, MD5 digest of the entries) and
the entries. Every entry becomes an `Entry.t`: list, id, version, party type
(person, entity, vessel, aircraft), names with their kind, dates of birth (day, month,
year or range), ISO nationalities, programmes, listing date.

Two OFAC files in different formats carry the same ids; load one of them per directory
(`Lists.create` refuses an entry that appears twice). The UK OFSI consolidated list
closed on 28 January 2026 and the UK Sanctions List replaced it as the only UK source;
`Uk_csv` serves archived OFSI files. The UK Sanctions List is published by the FCDO on
GOV.UK ("The UK Sanctions List", static URL
`https://sanctionslist.fcdo.gov.uk/docs/UK-Sanctions-List.xml`, schema 4.33, format
guide "Format guide for the UK Sanctions List"); the same data also comes as CSV, ODS
and other formats, of which the XML is read. Both UK files list the same people under
different ids, so load one of them. On the live file of 15 October 2026 `Uk_xml` reads
6 408 designations (4 058 people, 1 667 entities, 683 ships, 19 652 names) in under a
second with a 71 MB peak.

`fixtures/` holds small sanitised excerpts in each format; every party in them is
invented. They are embedded in the library (`List_source.load_fixtures ()`) and installed
under `share/sanctions-screening/fixtures/`.

## Normalisation and disguised names

A screening library is only as good as its weakest spelling: anything the normaliser
deletes or splits is a way to slip a listed name past it. The rules:

- **Nothing that is a letter or digit is ever deleted.** Letters of scripts without a
  Latinisation table (Georgian, Hebrew, Devanagari, CJK, ...) are kept as they are and
  left to fuzzy matching. Only punctuation and symbols separate words.
- **Invisible code points are removed, not turned into separators.** Every
  Default_Ignorable_Code_Point (zero-width space, joiners and non-joiners, soft hyphen,
  word joiner, byte order mark, bidi controls, variation selectors, tags) is dropped, so
  `Ole<ZWSP>g` reads `oleg`, not `ole g`.
- **Compatibility forms fold to plain letters.** NFKD covers fullwidth letters, ligatures,
  circled and mathematical alphanumerics (outside the BMP too), then combining marks are
  removed and the text lowercased.
- **Look-alikes are read both ways.** A word that mixes Latin with Cyrillic, Greek or
  Armenian letters, a word made only of non-Latin look-alikes of Latin letters, or a word
  that mixes letters and digits gets a second, visual reading (Cyrillic `р` as `p`, `0` as
  `o`) next to the transliterated one. Screening matches every reading and keeps the best
  score, so a disguise can only add hits, never remove one.
- **Separators cannot split a name away.** Each key also keeps the name's words joined
  without separators (`compact`), so `O.l.e.g P-a-r-a-m-o-n-o-v` and `Para monov` still
  compare equal to the listed spelling.

`test/adversarial.ml` lists the disguises the suite checks (homoglyphs, zero-width
characters, soft hyphens, bidi controls, fullwidth and mathematical letters, diacritics,
combining marks, mixed scripts, punctuation, spacing and digits); every one must still
produce a hit.

## Matching and scores

All scores are integers per mille (0 to 1000), computed with integer arithmetic, so they
are identical on every platform and can be pinned in tests and stored as evidence.

For one subject name against one list name (both normalised, `Match.score`):

| Component | Computed on | Notes |
| --- | --- | --- |
| `edit` | sorted tokens | Damerau-Levenshtein (optimal string alignment) as `1000 - 1000 * d / longer` |
| `compact` | words joined without separators | `edit` again, so split and joined words agree |
| `token_set`, `token_cover` | tokens | Monge-Elkan style alignment: each token of the shorter name takes its most similar unused token of the other by Jaro-Winkler (an initial scores 900 against words with its letter; pairs below `token_floor` 850 count 0), summed and divided by the shorter (`token_set`) or longer (`token_cover`) token count. Unmatched patronymics of the longer name do not count |
| `phonetic` | tokens | shared American Soundex codes over the longer token count, capped at 880 |
| `skeleton` | consonant skeletons | Jaro-Winkler, only when a side was written in Arabic script (short vowels are not written), capped at 930 |
| `jaro_winkler` | sorted tokens | reported for analysts, not used: a shared first word lifts unrelated names too much |

The name score is the maximum of `edit`, `compact`, the token score
(`0.6 * token_set + 0.4 * token_cover`) and the capped phonetic and skeleton scores. Per
entry the best name and reading count.

A name score at or above `name_threshold` (850 for onboarding, 870 for payments) makes a
hit. Secondary attributes then adjust its total and priority (`Policy.Adjustments`):

| Reason | Default |
| --- | --- |
| date of birth: exact / same month / same year / within 1 year / mismatch | +60 / +40 / +25 / +5 / -120 |
| nationality: shared / different (both known) | +20 / -40 |
| party type differs (person against company) | -150 |
| matched name is a low-quality alias | -50 |

A hit keeps everything an analyst needs: list, entry id, list version, both spellings,
every component, the reasons and the priority (`High` from 920, `Medium` from the
threshold, `Low` below it). By default secondary attributes never remove a hit: a strong
name with a different date of birth stays a `Low` hit, because lists carry partial and
wrong dates (`secondary_can_clear` changes that).

`Decision.t` is `Clear`, `Potential_match hits` or `Confirmed hits`. `Confirmed` needs a
`Policy.confirm` rule (off by default): name score at least its minimum, an exact date of
birth and no conflicting reason. `Decision.customer_view` turns anything but `Clear` into
a neutral `Under_review`, so nothing about alerts reaches the customer (tipping-off).

## Index

`Index` breaks every list name into character trigrams of its tokens (with boundary marks)
and of its separator-free form, plus consonant-skeleton trigrams. A query counts the
trigrams each name shares and scores only names whose Dice overlap reaches
`Policy.prefilter` (200 per mille). On the fixture probes (every list name, truncated,
reversed, and every disguise in `test/adversarial.ml`) the prefilter loses none of the 729
names a full scan scores at 850 or more (`test/test_screening.ml`).

## Screening, rescreening and cases

- `Screening.screen ~policy ~lists ~context ~at subject` screens every name of the subject
  in every reading. `Context.t` is `Onboarding`, `Data_change`, `Periodic`,
  `List_update` or `Counterparty { role = Originator | Beneficiary | Payee; reference }`.
  The `Result.t` carries an id derived from the inputs (subject, context, list digests,
  policy digest, time), the list versions and the policy digest, so it can be replayed.
- `Screening.rescreen ~policy ~before ~after ~at population` diffs the lists, screens
  the population against an index of the added and changed entries only, and screens in
  full just the subjects the delta can affect (a delta hit, or a previous hit on a
  removed or changed entry). It returns the diff per list, the newly hit subjects (a hit
  on an entry they did not hit before) and the cleared ones.
- `Case` is a pure state machine: `open_` on a potential match, `Assign`, `Decide`
  (true or false positive, with a reason), `Approve` / `Reject` by a second analyst,
  `Escalate`, `Note`. Four eyes are required for false positives by default
  (`Four_eyes.False_positive_only`; also `Off` and `All_decisions`). Every command
  returns the new record and its events; `Case.of_events` rebuilds a case from its trail.

## Storage

`Store.S` is the persistence a screening service needs, in domain types: list versions,
the subject population, screening results (by idempotency key), case events (by
idempotency key and expected version, so two analysts cannot act on the same state) and
open case ids. `Store_memory` serves tests and offline mode; `Store_pg`
(`sanctions-screening-pg`) keeps the same data in PostgreSQL (tables prefixed
`sanctions_`, schema in `migrations/`, embedded and applied by `Store_pg.connect`, bodies
as bin_prot of the `Stable` types).

The library owns one PostgreSQL schema, `sanctions_screening` by default
(`Store_pg.connect ~schema`): it creates the schema, and its tables and its own
`schema_migrations` table live inside it. Nothing is written outside it, so the host
application's migrations table and other libraries (transaction-monitoring keeps its own
`transaction_monitoring.schema_migrations`) never clash, though all start at version 1.
A database set up by 0.1.0 (tables and the bookkeeping row in the search path, usually
`public`) is adopted on connect: the six tables move into the schema and migration 1 is
recorded there; the old row in the shared table may then be deleted by hand. `Store_conformance` runs one scenario list against
any store; the tests run it on both and require the same transcript. No SQL appears
outside `pg/store_pg.ml` and `migrations/`.

## Wiring it into a service

Onboarding (one `Lists.t` per list publication, rebuilt by the job that fetches lists):

```ocaml
let onboard (module S : Store.S with type t = 'a) store ~lists ~customer ~now =
  let result =
    Screening.screen ~policy:Policy.default ~lists ~context:Onboarding ~at:now customer
  in
  let%bind _ = S.save_subject store customer in
  let%bind _ = S.save_result store ~idempotency_key:("onboarding:" ^ customer.id) result in
  match result.decision with
  | Clear -> return `Approve_screening
  | Potential_match _ | Confirmed _ ->
    let case, events = Case.open_ ~id:("case:" ^ result.id) ~at:now result |> ok_exn in
    let%map _ =
      S.append_case_events store ~idempotency_key:case.id ~expected_version:0 events
    in
    (* Record the hits on the KYC case too; the customer only sees "under review". *)
    `Hold_for_review (List.map (Decision.hits result.decision) ~f:Hit.to_kyc)
;;
```

`Hit.to_kyc` and `Screening.Context.to_kyc` give kyc-core's `Screening.Hit.t` and
`Screen_reason.t`, so `Kyc_core.Screening.create` can record the run and block approval
until every hit is discounted. The kyc-core hit's `entry_id` is the list-qualified
`Hit.entry_key` (`ofac-sdn:9001`), because kyc-core keys hits by entry id within a
screening and ids repeat across lists; the list's own id stays in `Hit.t.entry_id` and
`Hit.split_entry_key` recovers it.

Payee creation and payments: screen the counterparty name (and the account's or
address country) with `Policy.payments`, for SEPA with the end-to-end id as reference:

```ocaml
let check_beneficiary ~lists ~now ~payee_id ~name ~iban_country ~end_to_end_id =
  let subject = Subject.counterparty ~id:payee_id ~name ~country:iban_country () |> ok_exn in
  Screening.screen
    ~policy:Policy.payments
    ~lists
    ~context:(Counterparty { role = Beneficiary; reference = end_to_end_id })
    ~at:now
    subject
;;
```

A potential match on a payment puts the payment on hold (a ledger hold, not a rejection)
and opens a case; a confirmed match blocks it. For incoming SEPA payments screen the
originator (`role = Originator`) before crediting. When a list is published, run
`Screening.rescreen` over the stored subjects (`S.subjects`) with their latest decisions
(`S.latest_result`) and open cases for the newly hit.

## Numbers

Precision and recall on `test/labelled.tsv` (84 spellings of fixture entries: typos,
reordering, missing middle names and patronymics, initials, transliterations, Cyrillic,
Greek and Arabic script, legal forms, split and joined words, former names; and 44 other
names, many sharing words with listed ones), default policy without dates of birth:

| threshold | recall (queries) | recall (entries) | false alerts | hit precision |
| --- | --- | --- | --- | --- |
| 800 | 100.0% (84/84) | 98.7% (155/157) | 11.4% (5/44) | 95.1% |
| 825 | 100.0% (84/84) | 97.5% (153/157) | 2.3% (1/44) | 98.7% |
| **850 (default)** | **100.0% (84/84)** | **97.5% (153/157)** | **2.3% (1/44)** | **98.7%** |
| 875 | 96.4% (81/84) | 94.9% (149/157) | 2.3% (1/44) | 98.7% |
| 900 | 96.4% (81/84) | 92.4% (145/157) | 2.3% (1/44) | 98.6% |

The set is small and was used to choose the weights, so these numbers show the behaviour,
not a guarantee; a bank tunes `name_threshold` on its own alert budget. The one false
alert at 850 is a surname that is a prefix of a listed one. Every disguise in
`test/adversarial.ml` scores at least 975.

Speed (Apple M-series laptop, `bench/bench.exe`, `--profile release`; the dev profile and
`-O3` give the same within 10%, except Jaro-Winkler at 107 ns under `-O3`; the real-list
row was measured with `-O3`):

| | time | minor words |
| --- | --- | --- |
| screen one name, synthetic list of 20 000 entries (55 358 names) | 0.2 to 2.6 ms | 1 300 to 2 200 |
| screen one name, the real OFAC, EU and UK lists (30 826 entries, 94 650 names) | 0.3 to 0.7 ms | 1 300 to 2 100 |
| full scan of the 20 000 synthetic entries without the index | 72 ms | 24 |
| `Match.score`, one name pair | 1.5 us | 0 (`[@zero_alloc]`) |
| `Match.jaro_winkler`, `Match.damerau_levenshtein` | 67 ns, 152 ns | 0 (`[@zero_alloc]`) |
| `Normalize.keys`, a Cyrillic name | 7 us | 1 200 |
| load and index the real lists | 0.5 s and 0.9 s | |

`SANCTIONS_BENCH_LISTS=<dir> bench.exe` runs the same benchmark against fetched lists.

Peak memory (maximum resident set size of `sanctions-screening stats -lists <dir>`: load
and index; release build, Apple M-series laptop, lists of 15 October 2026):

| Lists | 0.1.0 | now |
| --- | --- | --- |
| OFAC advanced XML (128 MB) | 883 MB | 158 MB |
| OFAC legacy CSV (7 MB) | 126 MB | 114 MB |
| OFAC XML, EU XML, UK OFSI CSV | 916 MB | 329 MB |
| UK Sanctions List XML (22 MB) | | 71 MB |

The OFAC XML is read from disk a megabyte at a time (`Xml.iter_elements_channel`), each
party is reduced to the fields an entry uses as soon as it is read, with repeated ids
shared, and other files are read in one allocation of their size (`In_channel.read_all`
grows a buffer and held about three times the file at its peak).
The comparison primitives, the scorer, the token alignment and the UTF-8 decoder carry
`[@zero_alloc]` in their interfaces and the compiler checks them;
`test/test_match.ml` pins 0 words per `Match.score` call.


## Tests

`dune build @runtest` runs expect tests for every module, Quickcheck properties
(normalisation idempotence and word-order invariance, similarity symmetry and bounds,
case replay and four eyes on random command streams), the parsers against every fixture,
the adversarial spellings, the prefilter check, the precision and recall harness, the
store conformance on memory and on PostgreSQL (a throwaway cluster from
`pg-async.testing`; needs `initdb` and `pg_ctl` on the machine), the `Stable` digests and
a cram walk of the command. No test downloads anything.

## Known gaps

- The UN consolidated list has no parser yet; `List_id` already names it.
- The prefilter can miss a cross-script match: with an Arabic-script list name, a
  truncated or reversed Latin probe (`Fareed QUDAI`) scored 863 to 900 on the consonant
  skeleton while sharing too few trigrams to pass the prefilter. Found while writing the
  UK Sanctions List fixture; the committed fixtures have no such pair, so the prefilter
  check reads 0 missed.
- Double Metaphone is not implemented; Soundex is the phonetic key, capped so it cannot
  raise a hit on its own above 880.
- Greek is transliterated letter by letter except ου; Arabic short vowels are missing by
  nature of the script and are handled by the consonant skeleton.
- Addresses and identity documents on the lists are not parsed or compared.
- The weights were tuned on a small invented set; tune thresholds on real alert data.

## Docs

`AGENTS.md` (layout, commands, conventions for agents), `CHANGES.md`, the odoc of each
module, and `../specs/repos/sanctions-screening.md` in the workspace.
