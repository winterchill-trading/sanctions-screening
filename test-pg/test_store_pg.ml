open! Core
open! Async
open Sanctions_screening
module Pg = Store_conformance.Make (Sanctions_screening_pg.Store_pg)
module Memory = Store_conformance.Make (Store_memory)

(* A fresh database on pg-async's throwaway cluster (initdb + pg_ctl on a free port). *)
let fresh_url () =
  let%bind cluster = Pg_test.shared () in
  let%map config = Pg_test.create_database cluster in
  sprintf
    "postgres://postgres@127.0.0.1:%d/%s?sslmode=disable"
    (Pg_test.port cluster)
    (Option.value_exn config.dbname)
;;

let%expect_test "PostgreSQL store gives the memory store's transcript" =
  let%bind url = fresh_url () in
  let%bind store = Sanctions_screening_pg.Store_pg.connect ~url () >>| ok_exn in
  let migrations = Sanctions_screening_pg.Store_pg.applied_migrations store in
  let%bind () = Pg.run store in
  let pg = [%expect.output] in
  let%bind () = Memory.run (Store_memory.create ()) in
  let memory = [%expect.output] in
  print_s [%sexp (migrations : int list)];
  print_endline
    (if String.equal pg memory then "same transcript" else pg ^ "---\n" ^ memory);
  let%bind () = Sanctions_screening_pg.Store_pg.close store in
  [%expect {|
    (1)
    same transcript
    |}];
  return ()
;;
