open! Core
open Sanctions_screening
module Memory = Store_conformance.Make (Store_memory)

let%expect_test "memory store conformance" =
  Async.Thread_safe.block_on_async_exn (fun () -> Memory.run (Store_memory.create ()));
  [%expect
    {|
    save v1: Created
    save v1 again: Duplicate
    save v1 with other content: refused
    save v0: Created
    versions: v0,v1
    versions of another list: 0
    save c-2: ok
    save c-1: ok
    replace c-2: ok
    subjects: c-1=Oleg Paramonov,c-2=Maria Garcia Lopez
    save result: Created
    save result again: Duplicate
    reuse key for another result: refused
    save later result: Created
    find result: true
    find missing result: true
    latest result is the later one: true
    latest result of unknown subject: true
    open case: Created
    open case again: Duplicate
    open case with another key: refused
    assign: Created
    assign from a stale version: refused
    open cases: case-1
    decide: Created
    events: 1,2,3,4
    case rebuilt: true
    open cases after closing: []
    unknown case: true
    |}]
;;
