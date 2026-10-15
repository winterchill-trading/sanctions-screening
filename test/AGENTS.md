# test: expect tests and properties

One `test_<module>.ml` per area, plus:

- `adversarial.ml`: disguised spellings that must still hit (used by two tests).
- `labelled.tsv` and `test_precision_recall.ml`: the numbers quoted in the README.
- `fixture_lists.ml`: loads `../fixtures` once.
- `cli.t/run.t`: cram walk of the command.

Run `dune build --root .. @sanctions-screening/runtest`; promote with `dune promote` from
the workspace root (same `DUNE_BUILD_DIR`), after rerunning once if you edited a test
since the last run. Read every promoted diff: a scoring change moves many outputs.
