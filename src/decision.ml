open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Clear
      | Potential_match of Hit.Stable.V1.t list
      | Confirmed of Hit.Stable.V1.t list
    [@@deriving bin_io, compare, equal, sexp]
  end
end

type t = Stable.V1.t =
  | Clear
  | Potential_match of Hit.t list
  | Confirmed of Hit.t list
[@@deriving compare, equal, sexp]

let hits = function
  | Clear -> []
  | Potential_match hits | Confirmed hits -> hits
;;

module Customer_view = struct
  type t =
    | No_action_needed
    | Under_review
  [@@deriving sexp, compare, equal]
end

let customer_view : t -> Customer_view.t = function
  | Clear -> No_action_needed
  | Potential_match _ | Confirmed _ -> Under_review
;;

let to_string_hum t =
  match t with
  | Clear -> "clear"
  | Potential_match hits | Confirmed hits ->
    let label =
      match t with
      | Confirmed _ -> "confirmed"
      | _ -> "potential match"
    in
    String.concat
      ~sep:"\n"
      (label :: List.map hits ~f:(fun h -> "  " ^ Hit.to_string_hum h))
;;
