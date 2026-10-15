(** An in-memory {!Store.S}, with the same ordering and conflict rules as the
    PostgreSQL store. *)

open! Core

include Store.S

val create : unit -> t
