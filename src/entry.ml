open! Core

module Name = struct
  module Kind = struct
    type t =
      | Primary
      | Alias
      | Weak_alias
      | Former
    [@@deriving bin_io, compare, equal, hash, sexp]
  end

  type t =
    { name : string
    ; kind : Kind.t
    }
  [@@deriving bin_io, compare, equal, hash, sexp]
end

module Stable = struct
  module V1 = struct
    type t =
      { list : List_id.Stable.V1.t
      ; id : string
      ; version : string
      ; party : Party_type.Stable.V1.t
      ; names : Name.t list
      ; dobs : Dob.Stable.V1.t list
      ; nationalities : string list
      ; programmes : string list
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

let create
  ~list
  ~id
  ~version
  ~party
  ~names
  ?(dobs = [])
  ?(nationalities = [])
  ?(programmes = [])
  ?listed_on
  ()
  =
  let names =
    let seen = String.Hash_set.create () in
    List.filter_map names ~f:(fun (n : Name.t) ->
      let name = String.strip n.name in
      if String.is_empty name || Hash_set.mem seen name
      then None
      else (
        Hash_set.add seen name;
        Some { n with name }))
  in
  let names =
    (* A primary name goes first; the sort is stable, so the rest keep list order. *)
    List.stable_sort names ~compare:(fun (a : Name.t) (b : Name.t) ->
      let rank (k : Name.Kind.t) =
        match k with
        | Primary -> 0
        | Alias -> 1
        | Former -> 2
        | Weak_alias -> 3
      in
      Int.compare (rank a.kind) (rank b.kind))
  in
  if List.is_empty names
  then Or_error.error_s [%message "entry without a name" (list : List_id.t) id]
  else if String.is_empty (String.strip id)
  then Or_error.error_s [%message "entry without an id" (list : List_id.t)]
  else
    Ok
      { list
      ; id = String.strip id
      ; version
      ; party
      ; names
      ; dobs = List.dedup_and_sort dobs ~compare:Dob.compare
      ; nationalities =
          List.map nationalities ~f:(fun c -> String.uppercase (String.strip c))
          |> List.filter ~f:(Fn.non String.is_empty)
          |> List.dedup_and_sort ~compare:String.compare
      ; programmes =
          List.map programmes ~f:String.strip
          |> List.filter ~f:(Fn.non String.is_empty)
          |> List.dedup_and_sort ~compare:String.compare
      ; listed_on
      }
;;

let key t = List_id.to_string t.list ^ ":" ^ t.id
let primary_name t = (List.hd_exn t.names).name
let same_content a b = equal { a with version = "" } { b with version = "" }
