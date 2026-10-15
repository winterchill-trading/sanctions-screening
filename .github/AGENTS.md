# .github: CI

`workflows/ci.yml`: apt `libpq-dev postgresql`, infra `ox-setup` with pins `kyc-core`
then `pg-async`, `shellcheck`, build, tests, odoc with warnings as errors, and the
`obj-magic` reusable job. Keep the pins in dependency order and in step with
`dune-project`. Check runs with `gh run list --limit 1` and at most one
`gh run watch <id> --exit-status`.
