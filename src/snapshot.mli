(** The parsed content of one list publication. *)

open! Core

type t = private
  { version : List_version.t
  ; entries : Entry.t list (** sorted by id *)
  }
[@@deriving sexp_of]

(** Sorts the entries by id, fails on a duplicate id or an entry from another list, and
    computes the digest and count. *)
val create
  :  list:List_id.t
  -> version:string
  -> ?published:Date.t
  -> Entry.t list
  -> t Or_error.t

(** Entries added, removed and changed from [before] to [after] (by id; a changed entry is
    given in its new form). *)
module Diff : sig
  type t =
    { added : Entry.t list
    ; removed : Entry.t list
    ; changed : (Entry.t * Entry.t) list (** before, after *)
    }
  [@@deriving sexp_of]

  val is_empty : t -> bool
end

val diff : before:t -> after:t -> Diff.t
