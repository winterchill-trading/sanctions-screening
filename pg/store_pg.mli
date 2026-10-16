(** {!Sanctions_screening.Store.S} on PostgreSQL through pg-async.

    The library owns one schema (default [sanctions_screening], see {!connect}): its
    tables (prefixed [sanctions_], from [migrations/], embedded in the library) and the
    [schema_migrations] table recording which migrations were applied all live inside it.
    Nothing is created outside it, so the host application's own migrations table and
    other libraries' schemas never clash with these, even at the same version numbers.
    Nothing here joins with other services' tables. Writes run in serializable
    transactions, which pg-async retries on serialization failures. *)

open! Core
open! Async
include Sanctions_screening.Store.S

(** The schema used when {!connect} is not given one: ["sanctions_screening"]. *)
val default_schema : string

(** Connects (to [url], or [DATABASE_URL] / [PG*] variables when absent), creates [schema]
    if needed, applies the migrations inside it and opens a pool.

    [schema] must be a lower-case identifier ([[a-z_][a-z0-9_]*], not [pg_*]). Two
    instances may share a database under different schemas.

    A database set up by 0.1.0 (tables in the search path, usually [public], and the
    bookkeeping row in the shared [schema_migrations]) is adopted: the six tables move
    into [schema] and migration 1 is recorded there instead of being applied again. *)
val connect
  :  ?max_size:int
  -> ?url:string
  -> ?schema:string
  -> unit
  -> t Or_error.t Deferred.t

(** The schema this store lives in. *)
val schema : t -> string

val close : t -> unit Deferred.t

(** Migration versions applied by {!connect}. *)
val applied_migrations : t -> int list
