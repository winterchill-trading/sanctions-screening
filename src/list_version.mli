(** One publication of one list. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      { list : List_id.Stable.V1.t
      ; version : string
      (** the publisher's id or date of the publication, e.g. ["2026-10-15"] *)
      ; published : Date.Stable.V1.t option
      ; entries : int
      ; digest : string (** MD5 of the entries' bin_prot encoding, hex *)
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { list : List_id.t
  ; version : string
  ; published : Date.t option
  ; entries : int
  ; digest : string
  }
[@@deriving compare, equal, hash, sexp]
