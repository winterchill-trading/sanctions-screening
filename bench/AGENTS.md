# bench: timings

`bench.exe` (core_bench) screens against `Synthetic.lists ~n:20_000` or, with
`SANCTIONS_BENCH_LISTS=<dir>`, against real lists fetched by `../scripts/fetch-lists.sh`.
Build with `--profile release` (and once with `OCAMLPARAM='_,O3=1'`) and update the
README table when numbers move. Allocation counts are exact; times under 0.5 ns are noise.
