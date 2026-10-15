(* The committed fixture lists, loaded once for every test file that needs them. *)

open! Core
open Sanctions_screening

let snapshots = lazy (List_source.load_dir "../fixtures" |> ok_exn)
let lists = lazy (Lists.create (force snapshots) |> ok_exn)
let at = Time_ns.of_string_with_utc_offset "2026-10-15 12:00:00Z"

let screen ?(policy = Policy.default) ?(context = Screening.Context.Onboarding) subject =
  Screening.screen ~policy ~lists:(force lists) ~context ~at subject
;;

let person ?dob ?(nationalities = []) name =
  Subject.create
    ~id:"subject-1"
    ~party:Person
    ~names:[ name ]
    ?dob:(Option.map dob ~f:(fun d -> Dob.of_string d |> ok_exn))
    ~nationalities
    ()
  |> ok_exn
;;
