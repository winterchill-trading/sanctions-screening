(** Country names as lists print them, to ISO 3166 alpha-2 codes. *)

open! Core

(** ["Iran"], ["IRAN"], ["Russian Federation"] or ["ru"] to [Some "IR"] / [Some "RU"];
    matching is by {!Normalize.fold}, so case and accents do not matter. *)
val of_name : string -> string option
