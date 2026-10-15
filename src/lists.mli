(** The lists a screening runs against: one or more snapshots and their shared index. *)

open! Core

type t

(** Fails when two snapshots share a list and an entry id. Build once per list update and
    share it; building indexes every name. *)
val create : Snapshot.t list -> t Or_error.t

val snapshots : t -> Snapshot.t list
val versions : t -> List_version.t list
val index : t -> Index.t

(** By {!Entry.key}. *)
val find : t -> string -> Entry.t option

val entry_count : t -> int
