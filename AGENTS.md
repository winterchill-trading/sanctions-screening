# sanctions-screening: agent guide

## Purpose

An OCaml library that screens names of customers and payment counterparties against public
sanctions lists (OFAC SDN, EU consolidated list, UK) and returns explainable, deterministic
decisions. Work in progress: this file grows with the code.

## Build, test

Run from this repository, inside the workspace:

    opam exec --switch 5.2.0+ox -- dune build --root .. @sanctions-screening/all
    opam exec --switch 5.2.0+ox -- dune build --root .. @sanctions-screening/runtest

## Never do

- Run `opam install`, `upgrade` or `pin` that would change `ocaml-variants`, `core`, `base`,
  `async` or `ppx_jane` on the shared `5.2.0+ox` switch.
- Publish, tag or release.
- Download sanctions lists in tests; tests read the committed fixtures only.
