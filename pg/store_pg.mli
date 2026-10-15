(** {!Sanctions_screening.Store.S} on PostgreSQL through pg-async.

    The tables (prefixed [sanctions_]) come from [migrations/], embedded in the library
    and applied by {!connect}. Give the service its own database or schema
    ([search_path]); nothing here joins with other services' tables. Writes run in
    serializable transactions, which pg-async retries on serialization failures. *)

open! Core
open! Async

include Sanctions_screening.Store.S

(** Connects (to [url], or [DATABASE_URL] / [PG*] variables when absent), applies the
    migrations and opens a pool. *)
val connect : ?max_size:int -> ?url:string -> unit -> t Or_error.t Deferred.t

val close : t -> unit Deferred.t

(** Migration versions applied by {!connect}. *)
val applied_migrations : t -> int list
