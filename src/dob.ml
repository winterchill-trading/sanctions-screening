open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Day of Date.Stable.V1.t
      | Month of
          { year : int
          ; month : int
          }
      | Year of int
      | Years of
          { from : int
          ; until : int
          }
    [@@deriving bin_io, compare, equal, hash, sexp]
  end
end

type t = Stable.V1.t =
  | Day of Date.t
  | Month of
      { year : int
      ; month : int
      }
  | Year of int
  | Years of
      { from : int
      ; until : int
      }
[@@deriving compare, equal, hash, sexp]

let to_string = function
  | Day d -> Date.to_string d
  | Month { year; month } -> sprintf "%04d-%02d" year month
  | Year y -> sprintf "%04d" y
  | Years { from; until } -> sprintf "%04d..%04d" from until
;;

let of_string s =
  let s = String.strip s in
  let year y =
    match Int.of_string_opt y with
    | Some y when y >= 1000 && y <= 9999 -> Ok y
    | _ -> Or_error.error_s [%message "bad year" y]
  in
  match String.chop_prefix s ~prefix:"circa " with
  | Some y -> Result.map (year y) ~f:(fun y -> Years { from = y - 1; until = y + 1 })
  | None ->
    (match String.substr_index s ~pattern:".." with
     | Some i ->
       let open Or_error.Let_syntax in
       let%bind from = year (String.prefix s i) in
       let%bind until = year (String.drop_prefix s (i + 2)) in
       if from <= until
       then Ok (Years { from; until })
       else Or_error.error_s [%message "empty year range" s]
     | None ->
       (match String.split s ~on:'-' with
        | [ y ] -> Result.map (year y) ~f:(fun y -> Year y)
        | [ y; m ] ->
          let open Or_error.Let_syntax in
          let%bind year = year y in
          (match Int.of_string_opt m with
           | Some month when month >= 1 && month <= 12 -> Ok (Month { year; month })
           | _ -> Or_error.error_s [%message "bad month" s])
        | [ _; _; _ ] ->
          Or_error.try_with (fun () -> Day (Date.of_string s))
          |> Or_error.tag_s ~tag:[%message "bad date" s]
        | _ -> Or_error.error_s [%message "bad date of birth" s]))
;;

module Agreement = struct
  type t =
    | Exact
    | Same_month
    | Same_year
    | Within_years of int
    | Mismatch
  [@@deriving sexp, compare, equal, bin_io]

  (* Higher is stronger agreement. *)
  let rank = function
    | Exact -> 4
    | Same_month -> 3
    | Same_year -> 2
    | Within_years _ -> 1
    | Mismatch -> 0
  ;;
end

let years = function
  | Day d -> Date.year d, Date.year d
  | Month { year; _ } -> year, year
  | Year y -> y, y
  | Years { from; until } -> from, until
;;

let month_of = function
  | Day d -> Some (Date.year d, Month.to_int (Date.month d))
  | Month { year; month } -> Some (year, month)
  | Year _ | Years _ -> None
;;

let agree ~tolerance_years (a : t) (b : t) : Agreement.t =
  match a, b with
  | Day x, Day y when Date.equal x y -> Exact
  | _ ->
    (match month_of a, month_of b with
     | Some m1, Some m2 when [%equal: int * int] m1 m2 -> Same_month
     | _ ->
       let a0, a1 = years a
       and b0, b1 = years b in
       let gap = if a1 < b0 then b0 - a1 else if b1 < a0 then a0 - b1 else 0 in
       if gap = 0
       then Same_year
       else if gap <= tolerance_years
       then Within_years gap
       else Mismatch)
;;

let agreement ~tolerance_years subject entry_dobs =
  List.fold entry_dobs ~init:None ~f:(fun best d ->
    let a = agree ~tolerance_years subject d in
    match best with
    | Some b when Agreement.rank b >= Agreement.rank a -> best
    | _ -> Some a)
;;
