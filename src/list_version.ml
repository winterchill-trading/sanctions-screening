open! Core

module Stable = struct
  module V1 = struct
    type t =
      { list : List_id.Stable.V1.t
      ; version : string
      ; published : Date.Stable.V1.t option
      ; entries : int
      ; digest : string
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
