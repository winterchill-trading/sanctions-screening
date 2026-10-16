open! Core

let int_child el name =
  Option.bind (Xml.child el name) ~f:(fun c -> Int.of_string_opt (Xml.text c))
;;

(* A <From>/<To> pair inside <Start> of a DatePeriod. *)
let ymd el = int_child el "Year", int_child el "Month", int_child el "Day"

let dob_of_period period =
  let open Option.Let_syntax in
  let%bind start = Xml.child period "Start" in
  let%bind from = Xml.child start "From" in
  let%bind y0 = int_child from "Year" in
  let end_year =
    Option.bind (Xml.child period "End") ~f:(fun e ->
      Option.bind (Xml.child e "To") ~f:(fun t -> int_child t "Year"))
  in
  match ymd from, end_year with
  | _, Some y1 when y1 > y0 -> Some (Dob.Years { from = y0; until = y1 })
  | (Some y, Some m, Some d), _ ->
    Option.try_with (fun () -> Dob.Day (Date.create_exn ~y ~m:(Month.of_int_exn m) ~d))
  | (Some year, Some month, None), _ -> Some (Month { year; month })
  | _ -> Some (Year y0)
;;

(* What one DistinctParty contributes, reduced while streaming: only the fields the entry
   needs, still as the document's ids (reference values and locations are resolved after
   the pass, so the order of the sections does not matter). Holding these instead of each
   party's element tree took the peak heap of the 130 MB file from about 880 MB to the
   numbers in the README. *)
type raw_name =
  { primary : bool
  ; low_quality : bool
  ; alias_type : string
  ; parts : (string * string) list (* name part group id, text *)
  }

type raw_feature =
  { feature_type : string
  ; dobs : Dob.t list
  ; locations : string list
  }

type raw_party =
  { profile_id : string
  ; subtype : string
  ; groups : (string * string) list (* name part group id, name part type id *)
  ; names : raw_name list (* one per documented name, in document order *)
  ; features : raw_feature list
  }

type state =
  { mutable version : string
  ; refs : (string, (string * string) list) Hashtbl.t
      (* value set name -> (ID, text or attribute) *)
  ; location_country : (string, string) Hashtbl.t
  ; programmes : (string, string list) Hashtbl.t
  ; listed_on : (string, Date.t) Hashtbl.t
  ; parties : raw_party Queue.t
  ; interned : (string, string) Hashtbl.t
  }

(* Type and group ids repeat on every party (a few hundred distinct values over tens of
   thousands of uses); one shared copy of each keeps them from multiplying. *)
let intern st s =
  match Hashtbl.find st.interned s with
  | Some s -> s
  | None ->
    Hashtbl.set st.interned ~key:s ~data:s;
    s
;;

let raw_party st party =
  let%map.Option profile = Xml.child party "Profile" in
  let id el name = intern st (Xml.attr_or_empty el name) in
  let names =
    Xml.descendants profile "Alias"
    |> List.concat_map ~f:(fun alias ->
      let primary = String.equal (Xml.attr_or_empty alias "Primary") "true"
      and low_quality = String.equal (Xml.attr_or_empty alias "LowQuality") "true"
      and alias_type = id alias "AliasTypeID" in
      Xml.children alias "DocumentedName"
      |> List.map ~f:(fun dn ->
        { primary
        ; low_quality
        ; alias_type
        ; parts =
            List.map (Xml.descendants dn "NamePartValue") ~f:(fun p ->
              id p "NamePartGroupID", Xml.text p)
        }))
  in
  { profile_id = Xml.attr_or_empty profile "ID"
  ; subtype = id profile "PartySubTypeID"
  ; groups =
      List.map (Xml.descendants profile "NamePartGroup") ~f:(fun g ->
        id g "ID", id g "NamePartTypeID")
  ; names
  ; features =
      List.map (Xml.children profile "Feature") ~f:(fun f ->
        { feature_type = id f "FeatureTypeID"
        ; dobs = List.filter_map (Xml.descendants f "DatePeriod") ~f:dob_of_period
        ; locations =
            List.map (Xml.descendants f "VersionLocation") ~f:(fun vl ->
              id vl "LocationID")
        })
  }
;;

let parse_elements iter =
  let st =
    { version = "unknown"
    ; refs = String.Table.create ()
    ; location_country = String.Table.create ()
    ; programmes = String.Table.create ()
    ; listed_on = String.Table.create ()
    ; parties = Queue.create ()
    ; interned = String.Table.create ()
    }
  in
  let open Or_error.Let_syntax in
  let issue = Array.create ~len:3 None in
  let%bind () =
    iter ~f:(fun (el : Xml.t) ->
      match el.name with
      (* At this depth only DateOfIssue has Year, Month and Day children; reading them
         here keeps the parse to one pass that never builds a whole section. *)
      | "Year" -> issue.(0) <- Int.of_string_opt (Xml.text el)
      | "Month" -> issue.(1) <- Int.of_string_opt (Xml.text el)
      | "Day" -> issue.(2) <- Int.of_string_opt (Xml.text el)
      | "Location" ->
        Option.iter (Xml.child el "LocationCountry") ~f:(fun c ->
          Hashtbl.set
            st.location_country
            ~key:(intern st (Xml.attr_or_empty el "ID"))
            ~data:(intern st (Xml.attr_or_empty c "CountryID")))
      | "DistinctParty" -> Option.iter (raw_party st el) ~f:(Queue.enqueue st.parties)
      | "SanctionsEntry" ->
        let profile = Xml.attr_or_empty el "ProfileID" in
        Hashtbl.set st.programmes ~key:profile ~data:[];
        List.iter (Xml.children el "SanctionsMeasure") ~f:(fun m ->
          Hashtbl.add_multi
            st.programmes
            ~key:profile
            ~data:
              (Xml.attr_or_empty m "SanctionsTypeID"
               ^ "\000"
               ^ Option.value_map (Xml.child m "Comment") ~default:"" ~f:Xml.text));
        Option.iter (Xml.child el "EntryEvent") ~f:(fun ev ->
          Option.iter (Xml.child ev "Date") ~f:(fun d ->
            match ymd d with
            | Some y, Some m, Some d ->
              Option.iter
                (Option.try_with (fun () -> Date.create_exn ~y ~m:(Month.of_int_exn m) ~d))
                ~f:(fun date -> Hashtbl.set st.listed_on ~key:profile ~data:date)
            | _ -> ()))
      | name when String.is_suffix name ~suffix:"Values" ->
        Hashtbl.set
          st.refs
          ~key:name
          ~data:
            (List.filter_map el.children ~f:(function
              | Element v ->
                let id = Xml.attr_or_empty v "ID" in
                (* Countries are referred to by ISO2; party subtypes by their type. *)
                let value =
                  match name with
                  | "CountryValues" -> Xml.attr_or_empty v "ISO2"
                  | "PartySubTypeValues" ->
                    Xml.attr_or_empty v "PartyTypeID" ^ "\000" ^ Xml.text v
                  | _ -> Xml.text v
                in
                Some (id, value)
              | Text _ -> None))
      | _ -> ())
  in
  (match issue with
   | [| Some y; Some m; Some d |] -> st.version <- sprintf "%04d-%02d-%02d" y m d
   | _ -> ());
  let ref_table set =
    String.Map.of_alist_reduce
      (Option.value (Hashtbl.find st.refs set) ~default:[])
      ~f:(fun a _ -> a)
  in
  let countries = ref_table "CountryValues"
  and party_types = ref_table "PartyTypeValues"
  and subtypes = ref_table "PartySubTypeValues"
  and features = ref_table "FeatureTypeValues"
  and alias_types = ref_table "AliasTypeValues"
  and name_part_types = ref_table "NamePartTypeValues"
  and sanction_types = ref_table "SanctionsTypeValues" in
  let feature_named n =
    Map.filter features ~f:(String.equal n) |> Map.keys |> String.Set.of_list
  in
  let birth = feature_named "Birthdate"
  and nationality =
    Set.union (feature_named "Nationality Country") (feature_named "Citizenship Country")
  in
  let party_type subtype : Party_type.t =
    match Map.find subtypes subtype with
    | None -> Unknown
    | Some v ->
      let type_id, sub = String.lsplit2_exn v ~on:'\000' in
      (match Map.find party_types type_id with
       | Some "Individual" -> Person
       | Some "Entity" -> Entity
       | Some "Transport" -> if String.equal sub "Aircraft" then Aircraft else Vessel
       | _ -> Unknown)
  in
  let entries =
    Queue.to_list st.parties
    |> List.map ~f:(fun (raw : raw_party) ->
      let party = party_type raw.subtype in
      let last_name_groups =
        List.filter_map raw.groups ~f:(fun (group, part_type) ->
          match Map.find name_part_types part_type with
          | Some "Last Name" -> Some group
          | _ -> None)
        |> String.Set.of_list
      in
      let names =
        List.map raw.names ~f:(fun n ->
          let kind : Entry.Name.Kind.t =
            if n.primary
            then Primary
            else if n.low_quality
            then Weak_alias
            else (
              match Map.find alias_types n.alias_type with
              | Some "F.K.A." -> Former
              | _ -> Alias)
          in
          let last, rest =
            List.partition_tf n.parts ~f:(fun (group, _) ->
              Set.mem last_name_groups group)
          in
          let name = List.map (rest @ last) ~f:snd |> String.concat ~sep:" " in
          { Entry.Name.name; kind })
      in
      let of_type ids =
        List.filter raw.features ~f:(fun f -> Set.mem ids f.feature_type)
      in
      let dobs = of_type birth |> List.concat_map ~f:(fun f -> f.dobs) in
      let nationalities =
        of_type nationality
        |> List.concat_map ~f:(fun f -> f.locations)
        |> List.filter_map ~f:(fun location ->
          let%bind.Option country = Hashtbl.find st.location_country location in
          Map.find countries country)
        |> List.filter ~f:(Fn.non String.is_empty)
      in
      let programmes =
        Option.value (Hashtbl.find st.programmes raw.profile_id) ~default:[]
        |> List.filter_map ~f:(fun v ->
          let type_id, comment = String.lsplit2_exn v ~on:'\000' in
          match Map.find sanction_types type_id with
          | Some "Program" when not (String.is_empty comment) -> Some comment
          | _ -> None)
      in
      Entry.create
        ~list:Ofac_sdn
        ~id:raw.profile_id
        ~version:st.version
        ~party
        ~names
        ~dobs
        ~nationalities
        ~programmes
        ?listed_on:(Hashtbl.find st.listed_on raw.profile_id)
        ())
    |> Or_error.combine_errors
  in
  let%bind entries in
  Snapshot.create ~list:Ofac_sdn ~version:st.version entries
;;

let parse text = parse_elements (Xml.iter_elements text ~depth:2)
let of_channel ic = parse_elements (Xml.iter_elements_channel ic ~depth:2)
