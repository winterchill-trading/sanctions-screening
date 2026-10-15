open! Core

module Stable = struct
  module V1 = struct
    type t =
      { id : string
      ; party : Party_type.Stable.V1.t
      ; names : string list
      ; dob : Dob.Stable.V1.t option
      ; nationalities : string list
      }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  { id : string
  ; party : Party_type.t
  ; names : string list
  ; dob : Dob.t option
  ; nationalities : string list
  }
[@@deriving compare, equal, hash, sexp]

let create ~id ?(party = Party_type.Unknown) ~names ?dob ?(nationalities = []) () =
  let names = List.map names ~f:String.strip |> List.filter ~f:(Fn.non String.is_empty) in
  if String.is_empty (String.strip id)
  then Or_error.error_s [%message "subject without an id"]
  else if List.is_empty names
  then Or_error.error_s [%message "subject without a name" id]
  else
    Ok
      { id = String.strip id
      ; party
      ; names
      ; dob
      ; nationalities =
          List.map nationalities ~f:(fun c -> String.uppercase (String.strip c))
          |> List.dedup_and_sort ~compare:String.compare
      }
;;

let counterparty ~id ?party ~name ?country () =
  create ~id ?party ~names:[ name ] ?nationalities:(Option.map country ~f:List.return) ()
;;
