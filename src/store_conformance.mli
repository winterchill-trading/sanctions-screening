(** Scenarios every {!Store.S} must pass. [run] prints a transcript; a test runs it on
    each implementation and expects the same output. Use a fresh, empty store. *)

open! Core
open Async_kernel

module Make (S : Store.S) : sig
  val run : S.t -> unit Deferred.t
end
