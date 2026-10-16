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

let migrations_tables pool =
  Pg_async.Pool.fetch_all
    pool
    "SELECT table_schema::text AS s FROM information_schema.tables WHERE table_name = \
     'schema_migrations' ORDER BY 1"
    ~row:Pg_async.Row.(column "s" Pg_async.Type.string)
  >>| Pg_async.Pg_error.to_or_error
  >>| ok_exn
;;

(* The host application keeps its own [schema_migrations] (here in [public], at version 1
   like ours); the library records its migration inside its own schema, and a second
   instance under another schema does the same. *)
let%expect_test "migrations are recorded inside the library's schema" =
  let open Pg_async in
  let%bind url = fresh_url () in
  let config = Config.of_url url |> ok_exn in
  let%bind conn = Connection.connect config >>| Pg_error.ok_exn in
  let%bind host =
    Migrate.apply
      conn
      [ { version = 1; name = "init"; sql = "CREATE TABLE host_accounts (id text)" } ]
    >>| Result.map_error ~f:Migrate.Error.to_error
    >>| ok_exn
  in
  let%bind () = Connection.close conn in
  let module P = Sanctions_screening_pg.Store_pg in
  let%bind first = P.connect ~url () >>| ok_exn in
  let%bind second = P.connect ~url ~schema:"screening_two" () >>| ok_exn in
  let%bind again = P.connect ~url () >>| ok_exn in
  let%bind rejected = P.connect ~url ~schema:"x; DROP TABLE y" () in
  let pool = Pool.create ~max_size:1 config in
  let%bind tables = migrations_tables pool in
  print_s
    [%message
      ""
        (host : int list)
        ~first:((P.schema first, P.applied_migrations first) : string * int list)
        ~second:((P.schema second, P.applied_migrations second) : string * int list)
        ~again:(P.applied_migrations again : int list)
        (tables : string list)
        ~bad_name:(Result.is_error rejected : bool)];
  let%bind () = Pool.close pool in
  let%bind () = P.close first in
  let%bind () = P.close second in
  let%map () = P.close again in
  [%expect
    {|
    ((host (1)) (first (sanctions_screening (1))) (second (screening_two (1)))
     (again ()) (tables (public sanctions_screening screening_two))
     (bad_name true))
    |}]
;;

(* A database set up by 0.1.0: migration 1 applied in the search path with its row in the
   shared table, and a subject saved. Connecting moves the tables into the schema. *)
let%expect_test "an install from 0.1.0 is adopted" =
  let open Pg_async in
  let%bind url = fresh_url () in
  let config = Config.of_url url |> ok_exn in
  let%bind legacy =
    Migrate.load_dir "../migrations"
    >>| Result.map_error ~f:Migrate.Error.to_error
    >>| ok_exn
  in
  let%bind conn = Connection.connect config >>| Pg_error.ok_exn in
  let%bind old =
    Migrate.apply conn legacy >>| Result.map_error ~f:Migrate.Error.to_error >>| ok_exn
  in
  let subject =
    Subject.create ~id:"customer-1" ~party:Person ~names:[ "Ivan Petrov" ] () |> ok_exn
  in
  let%bind (_ : int) =
    Query.execute
      conn
      "INSERT INTO sanctions_subjects (id, body) VALUES ($1, $2)"
      ~params:
        [ Param.string subject.id
        ; Param.bytea (Bin_prot.Writer.to_string Subject.Stable.V1.bin_writer_t subject)
        ]
    >>| Pg_error.ok_exn
  in
  let%bind () = Connection.close conn in
  let module P = Sanctions_screening_pg.Store_pg in
  let%bind store = P.connect ~url () >>| ok_exn in
  let%bind subjects = P.subjects store >>| ok_exn in
  let pool = Pool.create ~max_size:1 config in
  let%bind tables = migrations_tables pool in
  let%bind left_in_public =
    Pool.fetch_all
      pool
      "SELECT table_name::text AS t FROM information_schema.tables WHERE table_schema = \
       'public' ORDER BY 1"
      ~row:Row.(column "t" Type.string)
    >>| Pg_error.to_or_error
    >>| ok_exn
  in
  print_s
    [%message
      ""
        (old : int list)
        ~applied_now:(P.applied_migrations store : int list)
        ~subjects:(List.map subjects ~f:(fun (s : Subject.t) -> s.id) : string list)
        (tables : string list)
        (left_in_public : string list)];
  let%bind () = Pool.close pool in
  let%map () = P.close store in
  [%expect
    {|
    ((old (1)) (applied_now ()) (subjects (customer-1))
     (tables (public sanctions_screening)) (left_in_public (schema_migrations)))
    |}]
;;
