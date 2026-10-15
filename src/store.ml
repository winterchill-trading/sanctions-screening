open! Core
open Async_kernel

module Write = struct
  type t =
    [ `Created
    | `Duplicate
    ]
  [@@deriving sexp, compare, equal]
end

module type S = sig
  type t

  val save_list_version : t -> List_version.t -> Write.t Or_error.t Deferred.t
  val list_versions : t -> List_id.t -> List_version.t list Or_error.t Deferred.t
  val save_subject : t -> Subject.t -> unit Or_error.t Deferred.t
  val subjects : t -> Subject.t list Or_error.t Deferred.t

  val save_result
    :  t
    -> idempotency_key:string
    -> Screening.Result.t
    -> Write.t Or_error.t Deferred.t

  val find_result : t -> string -> Screening.Result.t option Or_error.t Deferred.t

  val latest_result
    :  t
    -> subject_id:string
    -> Screening.Result.t option Or_error.t Deferred.t

  val append_case_events
    :  t
    -> idempotency_key:string
    -> expected_version:int
    -> Case.Event.t list
    -> Write.t Or_error.t Deferred.t

  val case_events : t -> case_id:string -> Case.Event.t list Or_error.t Deferred.t
  val open_case_ids : t -> string list Or_error.t Deferred.t
end

let find_case (type a) (module M : S with type t = a) (t : a) case_id =
  match%map M.case_events t ~case_id with
  | Error _ as e -> e
  | Ok [] -> Ok None
  | Ok events -> Or_error.map (Case.of_events events) ~f:Option.some
;;
