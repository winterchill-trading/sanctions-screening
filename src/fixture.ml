open! Core

module Spec = struct
  type entry =
    { id : string
    ; party : Party_type.t
    ; names : Entry.Name.t list
    ; dobs : Dob.t list [@sexp.list]
    ; nationalities : string list [@sexp.list]
    ; programmes : string list [@sexp.list]
    ; listed_on : Date.t option [@sexp.option]
    }
  [@@deriving sexp]

  type t =
    { list : List_id.t
    ; version : string
    ; published : Date.t option [@sexp.option]
    ; entries : entry list
    }
  [@@deriving sexp]
end

let parse text =
  let open Or_error.Let_syntax in
  let%bind spec = Or_error.try_with (fun () -> Spec.t_of_sexp (Sexp.of_string text)) in
  let%bind entries =
    List.map spec.entries ~f:(fun (e : Spec.entry) ->
      Entry.create
        ~list:spec.list
        ~id:e.id
        ~version:spec.version
        ~party:e.party
        ~names:e.names
        ~dobs:e.dobs
        ~nationalities:e.nationalities
        ~programmes:e.programmes
        ?listed_on:e.listed_on
        ())
    |> Or_error.combine_errors
  in
  Snapshot.create ~list:spec.list ~version:spec.version ?published:spec.published entries
;;

let to_string (s : Snapshot.t) =
  { Spec.list = s.version.list
  ; version = s.version.version
  ; published = s.version.published
  ; entries =
      List.map s.entries ~f:(fun (e : Entry.t) ->
        { Spec.id = e.id
        ; party = e.party
        ; names = e.names
        ; dobs = e.dobs
        ; nationalities = e.nationalities
        ; programmes = e.programmes
        ; listed_on = e.listed_on
        })
  }
  |> Spec.sexp_of_t
  |> Sexp.to_string_hum
;;
