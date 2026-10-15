(** One listed party, normalised from any list format. *)

open! Core

module Name : sig
  module Kind : sig
    type t =
      | Primary
      | Alias (** a.k.a., good-quality alias, name variation *)
      | Weak_alias (** low-quality alias: screened, but its hits rank lower *)
      | Former (** f.k.a. *)
    [@@deriving bin_io, compare, equal, hash, sexp]
  end

  type t =
    { name : string (** as published *)
    ; kind : Kind.t
    }
  [@@deriving bin_io, compare, equal, hash, sexp]
end

module Stable : sig
  module V1 : sig
    type t =
      { list : List_id.Stable.V1.t
      ; id : string (** the list's own id: OFAC ent_num, EU logicalId, UK Group ID *)
      ; version : string (** publication the entry was read from *)
      ; party : Party_type.Stable.V1.t
      ; names : Name.t list (** primary first *)
      ; dobs : Dob.Stable.V1.t list
      ; nationalities : string list (** ISO 3166 alpha-2, upper case, sorted *)
      ; programmes : string list (** sanctions programmes or regimes, sorted *)
      ; listed_on : Date.Stable.V1.t option
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { list : List_id.t
  ; id : string
  ; version : string
  ; party : Party_type.t
  ; names : Name.t list
  ; dobs : Dob.t list
  ; nationalities : string list
  ; programmes : string list
  ; listed_on : Date.t option
  }
[@@deriving compare, equal, hash, sexp]

(** Removes blank and duplicate names (keeping the first, so a primary name stays first),
    upper-cases, sorts and dedups nationalities, sorts programmes, dedups dates. Fails
    without any name. *)
val create
  :  list:List_id.t
  -> id:string
  -> version:string
  -> party:Party_type.t
  -> names:Name.t list
  -> ?dobs:Dob.t list
  -> ?nationalities:string list
  -> ?programmes:string list
  -> ?listed_on:Date.t
  -> unit
  -> t Or_error.t

(** ["<list>:<id>"], unique across lists. *)
val key : t -> string

(** The first name. *)
val primary_name : t -> string

(** Same entry content, ignoring [version] (what a list diff compares). *)
val same_content : t -> t -> bool
