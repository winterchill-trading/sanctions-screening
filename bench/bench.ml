open! Core
open Core_bench
open Sanctions_screening

let n = 20_000

(* SANCTIONS_BENCH_LISTS=<dir> benchmarks against real list files (from
   scripts/fetch-lists.sh) instead of the synthetic list. *)
let () =
  let t0 = Time_ns.now () in
  let lists =
    match Sys.getenv "SANCTIONS_BENCH_LISTS" with
    | Some dir -> Lists.create (List_source.load_dir dir |> ok_exn) |> ok_exn
    | None -> Synthetic.lists ~n
  in
  let built = Time_ns.diff (Time_ns.now ()) t0 in
  eprintf
    "index of %d entries, %d names, built in %s\n%!"
    (Lists.entry_count lists)
    (Index.name_count (Lists.index lists))
    (Time_ns.Span.to_string_hum built);
  let scratch = Match.Scratch.create () in
  let at = Time_ns.epoch in
  let entry = List.nth_exn (Synthetic.entries ~n:200) 123 in
  let listed = Entry.primary_name entry in
  let subject name = Subject.create ~id:"b" ~party:Person ~names:[ name ] () |> ok_exn in
  let hit = subject listed
  and typo = subject (String.drop_suffix listed 1 ^ "x")
  and miss = subject "Jonathan Livingstone Seagull" in
  let screen s () =
    ignore
      (Screening.screen ~scratch ~policy:Policy.default ~lists ~context:Onboarding ~at s
       : Screening.Result.t)
  in
  let a = Match.Prepared.of_key (Normalize.key "Oleg Ivanovich Paramonov")
  and b = Match.Prepared.of_key (Normalize.key "Paramonov Oleg") in
  let index = Lists.index lists in
  let q = Match.Prepared.of_key (Normalize.key listed) in
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"screen: listed name" (screen hit)
       ; Bench.Test.create ~name:"screen: typo of listed name" (screen typo)
       ; Bench.Test.create ~name:"screen: clear name" (screen miss)
       ; Bench.Test.create ~name:"index search: listed name" (fun () ->
           ignore
             (Index.search
                index
                scratch
                ~weights:Policy.default.weights
                ~prefilter:Policy.default.prefilter
                ~threshold:850
                q
              : (int * int) list))
       ; Bench.Test.create ~name:"full scan without index" (fun () ->
           ignore
             (Index.scan index scratch ~weights:Policy.default.weights ~threshold:850 q
              : (int * int) list))
       ; Bench.Test.create ~name:"Match.score one pair" (fun () ->
           ignore (Match.score scratch Match.Weights.default a b : int))
       ; Bench.Test.create ~name:"Match.jaro_winkler" (fun () ->
           ignore (Match.jaro_winkler scratch "paramonov" "paramanov" : int))
       ; Bench.Test.create ~name:"Match.damerau_levenshtein" (fun () ->
           ignore (Match.damerau_levenshtein scratch "paramonov" "paramanov" : int))
       ; Bench.Test.create ~name:"Normalize.keys (Cyrillic)" (fun () ->
           ignore
             (Normalize.keys ~party:Person "Олег Иванович Парамонов"
              : Normalize.Key.t list))
       ])
;;
