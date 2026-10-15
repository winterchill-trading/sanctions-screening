(** Persistence for a screening service, in domain types.

    Two implementations: {!Store_memory} here (tests, offline mode) and [Store_pg] in the
    [sanctions-screening-pg] package (PostgreSQL through pg-async). Both pass
    {!Store_conformance}. SQL lives only in that package and its [migrations/].

    Every write is idempotent: a write repeated with the same idempotency key and the same
    content answers [`Duplicate]; the same key with different content is an error. Reads
    never assume they see a write made through another store instance. *)

open! Core
open Async_kernel

module Write : sig
  type t =
    [ `Created
    | `Duplicate
    ]
  [@@deriving sexp, compare, equal]
end

module type S = sig
  type t

  (** Records a list publication; keyed by list and version. *)
  val save_list_version : t -> List_version.t -> Write.t Or_error.t Deferred.t

  (** Versions of one list, oldest first (by [version], byte order). *)
  val list_versions : t -> List_id.t -> List_version.t list Or_error.t Deferred.t

  (** Inserts or replaces the subject with this id (the population to rescreen). *)
  val save_subject : t -> Subject.t -> unit Or_error.t Deferred.t

  (** All subjects, by id. *)
  val subjects : t -> Subject.t list Or_error.t Deferred.t

  val save_result
    :  t
    -> idempotency_key:string
    -> Screening.Result.t
    -> Write.t Or_error.t Deferred.t

  val find_result : t -> string -> Screening.Result.t option Or_error.t Deferred.t

  (** The newest result for a subject (by time, then id). *)
  val latest_result
    :  t
    -> subject_id:string
    -> Screening.Result.t option Or_error.t Deferred.t

  (** Appends a command's events to a case. [expected_version] is the case's version the
      events were computed from (0 for a new case); a different stored version is a
      conflict, so two analysts cannot both act on the same state. *)
  val append_case_events
    :  t
    -> idempotency_key:string
    -> expected_version:int
    -> Case.Event.t list
    -> Write.t Or_error.t Deferred.t

  val case_events : t -> case_id:string -> Case.Event.t list Or_error.t Deferred.t

  (** Ids of cases that are not closed, by id. *)
  val open_case_ids : t -> string list Or_error.t Deferred.t
end

(** Rebuilds a case from a store's events; [None] when it has none. *)
val find_case
  :  (module S with type t = 'a)
  -> 'a
  -> string
  -> Case.t option Or_error.t Deferred.t
