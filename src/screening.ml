open! Core

module Context = struct
  module Role = struct
    type t =
      | Originator
      | Beneficiary
      | Payee
    [@@deriving bin_io, compare, equal, hash, sexp]
  end

  module Stable = struct
    module V1 = struct
      type t =
        | Onboarding
        | Data_change
        | Periodic
        | List_update
        | Counterparty of
            { role : Role.t
            ; reference : string
            }
      [@@deriving bin_io, compare, equal, hash, sexp]
    end
  end

  type t = Stable.V1.t =
    | Onboarding
    | Data_change
    | Periodic
    | List_update
    | Counterparty of
        { role : Role.t
        ; reference : string
        }
  [@@deriving compare, equal, hash, sexp]

  let to_kyc : t -> Kyc_core.Screening.Screen_reason.t = function
    | Onboarding -> Onboarding
    | Data_change -> Data_change
    | Periodic -> Periodic
    | List_update -> List_update
    | Counterparty _ -> Payment
  ;;
end

module Result = struct
  module Stable = struct
    module V1 = struct
      type t =
        { id : string
        ; subject : Subject.Stable.V1.t
        ; context : Context.Stable.V1.t
        ; decision : Decision.Stable.V1.t
        ; lists : List_version.Stable.V1.t list
        ; policy_digest : string
        ; at : Time_ns.Stable.Alternate_sexp.V1.t
        }
      [@@deriving bin_io, compare, equal, sexp]
    end
  end

  type t = Stable.V1.t =
    { id : string
    ; subject : Subject.t
    ; context : Context.t
    ; decision : Decision.t
    ; lists : List_version.t list
    ; policy_digest : string
    ; at : Time_ns.Alternate_sexp.t
    }
  [@@deriving compare, equal, sexp]

  let hit_keys t = String.Set.of_list (List.map (Decision.hits t.decision) ~f:Hit.entry_key)
end

let adjust (policy : Policy.t) (subject : Subject.t) (entry : Entry.t) (name : Entry.Name.t) =
  let a = policy.adjustments in
  let reasons = ref []
  and delta = ref 0 in
  let add r d =
    reasons := r :: !reasons;
    delta := !delta + d
  in
  (match subject.dob with
   | None -> ()
   | Some dob ->
     (match Dob.agreement ~tolerance_years:policy.dob_tolerance_years dob entry.dobs with
      | None -> ()
      | Some agreement ->
        add
          (Hit.Reason.Dob agreement)
          (match agreement with
           | Exact -> a.dob_exact
           | Same_month -> a.dob_same_month
           | Same_year -> a.dob_same_year
           | Within_years _ -> a.dob_within_tolerance
           | Mismatch -> a.dob_mismatch)));
  (match subject.nationalities, entry.nationalities with
   | [], _ | _, [] -> ()
   | s, e ->
     (match List.find s ~f:(fun c -> List.mem e c ~equal:String.equal) with
      | Some c -> add (Nationality_match c) a.nationality_match
      | None -> add (Nationality_mismatch { subject = s; entry = e }) a.nationality_mismatch));
  if not (Party_type.compatible subject.party entry.party)
  then add (Type_mismatch { subject = subject.party; entry = entry.party }) a.type_mismatch;
  (match name.kind with
   | Weak_alias -> add Weak_alias a.weak_alias
   | Primary | Alias | Former -> ());
  List.rev !reasons, !delta
;;

let id_of ~subject ~context ~lists ~policy ~at =
  let parts =
    [ Bin_prot.Writer.to_string Subject.Stable.V1.bin_writer_t subject
    ; Bin_prot.Writer.to_string Context.Stable.V1.bin_writer_t context
    ; String.concat ~sep:"," (List.map lists ~f:(fun (v : List_version.t) -> v.digest))
    ; Policy.digest policy
    ; Int63.to_string (Time_ns.to_int63_ns_since_epoch at)
    ]
  in
  Md5.to_hex (Md5.digest_string (String.concat ~sep:"\000" parts))
;;

(* The best (name score, name id, subject spelling, reading) per entry. *)
let candidates scratch (policy : Policy.t) (lists : Lists.t) (subject : Subject.t) =
  let index = Lists.index lists in
  let best = Int.Table.create () in
  List.iter subject.names ~f:(fun spelling ->
    List.iter (Normalize.keys ~party:subject.party spelling) ~f:(fun key ->
      if not (Normalize.Key.is_empty key)
      then (
        let q = Match.Prepared.of_key key in
        Index.search
          index
          scratch
          ~weights:policy.weights
          ~prefilter:policy.prefilter
          ~threshold:policy.name_threshold
          q
        |> List.iter ~f:(fun (id, score) ->
          let entry = (Index.name index id).entry in
          match Hashtbl.find best entry with
          | Some (s, _, _, _) when s >= score -> ()
          | _ -> Hashtbl.set best ~key:entry ~data:(score, id, spelling, q)))));
  best
;;

let screen ?scratch ~(policy : Policy.t) ~lists ~context ~at (subject : Subject.t) =
  let scratch =
    match scratch with
    | Some s -> s
    | None -> Match.Scratch.create ()
  in
  let index = Lists.index lists in
  let version_of =
    List.map (Lists.versions lists) ~f:(fun (v : List_version.t) -> v.list, v.version)
  in
  let hits =
    candidates scratch policy lists subject
    |> Hashtbl.to_alist
    |> List.filter_map ~f:(fun (entry_i, (_, id, spelling, q)) ->
      let n = Index.name index id in
      let entry = (Index.entries index).(entry_i) in
      let name = Match.explain scratch policy.weights q n.prepared in
      let reasons, adjustment = adjust policy subject entry n.name in
      let reasons =
        match n.prepared.key.reading, q.key.reading with
        | Visual, _ | _, Visual -> reasons @ [ Hit.Reason.Visual_reading ]
        | Phonetic, Phonetic -> reasons
      in
      let score = Int.clamp_exn (name.score + adjustment) ~min:0 ~max:1000 in
      let priority : Hit.Priority.t =
        if score >= policy.high_priority_at
        then High
        else if score >= policy.name_threshold
        then Medium
        else Low
      in
      if policy.secondary_can_clear && score < policy.name_threshold
      then None
      else
        Some
          { Hit.list = entry.list
          ; entry_id = entry.id
          ; list_version =
              Option.value
                (List.Assoc.find version_of entry.list ~equal:List_id.equal)
                ~default:entry.version
          ; party = entry.party
          ; matched_name = n.name.name
          ; subject_name = spelling
          ; name
          ; reasons
          ; adjustment
          ; score
          ; priority
          ; programmes = entry.programmes
          })
    |> List.sort ~compare:(fun (a : Hit.t) (b : Hit.t) ->
      match Int.compare b.score a.score with
      | 0 -> String.compare (Hit.entry_key a) (Hit.entry_key b)
      | c -> c)
    |> fun hits -> List.take hits policy.max_hits
  in
  let decision : Decision.t =
    match hits with
    | [] -> Clear
    | first :: _ ->
      let confirmed =
        match policy.confirm with
        | None -> false
        | Some rule ->
          first.name.score >= rule.min_name_score
          && List.mem first.reasons (Dob Exact) ~equal:Hit.Reason.equal
          && not
               (List.exists first.reasons ~f:(function
                 | Type_mismatch _ | Nationality_mismatch _ | Weak_alias -> true
                 | _ -> false))
      in
      if confirmed then Confirmed hits else Potential_match hits
  in
  let versions = Lists.versions lists in
  { Result.id = id_of ~subject ~context ~lists:versions ~policy ~at
  ; subject
  ; context
  ; decision
  ; lists = versions
  ; policy_digest = Policy.digest policy
  ; at
  }
;;

module Rescreen = struct
  type change =
    { subject : Subject.t
    ; previous : Decision.t
    ; result : Result.t
    }
  [@@deriving sexp_of]

  type t =
    { diff : (List_id.t * Snapshot.Diff.t) list
    ; newly_hit : change list
    ; cleared : change list
    ; rescreened : int
    ; unchanged : int
    }
  [@@deriving sexp_of]
end

let rescreen ?scratch ~policy ~before ~after ~at subjects =
  let open Or_error.Let_syntax in
  let scratch =
    match scratch with
    | Some s -> s
    | None -> Match.Scratch.create ()
  in
  let by_list lists =
    List.map (Lists.snapshots lists) ~f:(fun (s : Snapshot.t) -> s.version.list, s)
  in
  let b = by_list before
  and a = by_list after in
  let ids =
    List.map (b @ a) ~f:fst |> List.dedup_and_sort ~compare:List_id.compare
  in
  let%bind diff =
    List.map ids ~f:(fun list ->
      let empty version = Snapshot.create ~list ~version [] in
      let%bind before_s =
        match List.Assoc.find b list ~equal:List_id.equal with
        | Some s -> Ok s
        | None -> empty "none"
      in
      let%map after_s =
        match List.Assoc.find a list ~equal:List_id.equal with
        | Some s -> Ok s
        | None -> empty "none"
      in
      list, Snapshot.diff ~before:before_s ~after:after_s)
    |> Or_error.combine_errors
  in
  let touched_keys =
    List.concat_map diff ~f:(fun (_, (d : Snapshot.Diff.t)) ->
      List.map (d.removed @ List.map d.changed ~f:fst) ~f:Entry.key)
    |> String.Set.of_list
  in
  let%bind delta =
    List.map diff ~f:(fun (list, (d : Snapshot.Diff.t)) ->
      let version =
        Option.value_map
          (List.Assoc.find a list ~equal:List_id.equal)
          ~default:"none"
          ~f:(fun (s : Snapshot.t) -> s.version.version)
      in
      Snapshot.create ~list ~version (d.added @ List.map d.changed ~f:snd))
    |> Or_error.combine_errors
  in
  let%map delta = Lists.create delta in
  let delta_empty = Lists.entry_count delta = 0 in
  let affected (subject : Subject.t) previous =
    List.exists (Decision.hits previous) ~f:(fun h -> Set.mem touched_keys (Hit.entry_key h))
    || ((not delta_empty)
        && not (Hashtbl.is_empty (candidates scratch policy delta subject)))
  in
  let newly_hit = ref []
  and cleared = ref []
  and rescreened = ref 0
  and unchanged = ref 0 in
  List.iter subjects ~f:(fun (subject, previous) ->
    if not (affected subject previous)
    then incr unchanged
    else (
      incr rescreened;
      let result = screen ~scratch ~policy ~lists:after ~context:List_update ~at subject in
      let old_keys =
        String.Set.of_list (List.map (Decision.hits previous) ~f:Hit.entry_key)
      in
      let change = { Rescreen.subject; previous; result } in
      if not (Set.is_empty (Set.diff (Result.hit_keys result) old_keys))
      then newly_hit := change :: !newly_hit
      else if
        (not (Set.is_empty old_keys))
        && [%equal: Decision.t] result.decision Clear
      then cleared := change :: !cleared));
  { Rescreen.diff
  ; newly_hit = List.rev !newly_hit
  ; cleared = List.rev !cleared
  ; rescreened = !rescreened
  ; unchanged = !unchanged
  }
;;
