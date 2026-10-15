(** Deterministic synthetic lists for benchmarks. *)

open! Core
open Sanctions_screening

val entries : n:int -> Entry.t list
val lists : n:int -> Lists.t
