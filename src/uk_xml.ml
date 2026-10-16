open! Core

(* "23/08/1968", "dd/mm/1959", "dd/04/1960" or "1960". Anything else (the live file has a
   "11/11/19yy") is not a date and is skipped. *)
let parse_dob s : Dob.t option =
  let part p = if String.for_all p ~f:Char.is_alpha then None else Int.of_string_opt p in
  match String.split (String.strip s) ~on:'/' with
  | [ y ] -> Option.map (Int.of_string_opt y) ~f:(fun y -> Dob.Year y)
  | [ d; m; y ] ->
    (match part d, part m, Int.of_string_opt y with
     | _, _, None -> None
     | Some d, Some m, Some y when m >= 1 && m <= 12 ->
       Option.try_with (fun () -> Dob.Day (Date.create_exn ~y ~m:(Month.of_int_exn m) ~d))
     | None, Some m, Some year when m >= 1 && m <= 12 -> Some (Month { year; month = m })
     | None, None, Some y -> Some (Year y)
     | _ -> None)
  | _ -> None
;;

let parse_date s =
  match parse_dob s with
  | Some (Day d) -> Some d
  | _ -> None
;;

let text_of el name = Option.value_map (Xml.child el name) ~default:"" ~f:Xml.text

let name_of (n : Xml.t) : Entry.Name.t =
  let name =
    List.map [ "Name1"; "Name2"; "Name3"; "Name4"; "Name5"; "Name6" ] ~f:(text_of n)
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.concat ~sep:" "
  in
  let kind : Entry.Name.Kind.t =
    match String.lowercase (text_of n "NameType") with
    | "primary name" -> Primary
    | _ ->
      if String.is_prefix (String.lowercase (text_of n "AliasStrength")) ~prefix:"low"
      then Weak_alias
      else Alias
  in
  { name; kind }
;;

(* An entry with the version still to fill in: [DateGenerated] comes first in the
   published file, but nothing here depends on that. *)
let entry_of (d : Xml.t) =
  let party : Party_type.t =
    match String.lowercase (text_of d "IndividualEntityShip") with
    | "individual" -> Person
    | "entity" -> Entity
    | "ship" -> Vessel
    | _ -> Unknown
  in
  let names =
    List.concat_map (Xml.children d "Names") ~f:(fun ns -> Xml.children ns "Name")
    |> List.map ~f:name_of
  in
  let non_latin =
    Xml.descendants d "NameNonLatinScript"
    |> List.map ~f:(fun n -> { Entry.Name.name = Xml.text n; kind = Alias })
  in
  (* Primary names first, keeping the published order otherwise. *)
  let primary, other =
    List.partition_tf names ~f:(fun n -> Entry.Name.Kind.equal n.kind Primary)
  in
  Entry.create
    ~list:Uk_sanctions_list
    ~id:(text_of d "UniqueID")
    ~version:"unknown"
    ~party
    ~names:(primary @ other @ non_latin)
    ~dobs:(List.filter_map (Xml.descendants d "DOB") ~f:(fun e -> parse_dob (Xml.text e)))
    ~nationalities:
      (List.filter_map (Xml.descendants d "Nationality") ~f:(fun e ->
         Country.of_name (Xml.text e)))
    ~programmes:(List.map (Xml.children d "RegimeName") ~f:Xml.text)
    ?listed_on:(parse_date (text_of d "DateDesignated"))
    ()
  |> Or_error.tag_s ~tag:[%message "UK designation" ~id:(text_of d "UniqueID")]
;;

let parse_elements iter =
  let open Or_error.Let_syntax in
  let version = ref "unknown" in
  let entries = Queue.create () in
  let%bind () =
    iter ~f:(fun (el : Xml.t) ->
      match el.name with
      | "DateGenerated" ->
        version
        := (match parse_date (Xml.text el) with
             | Some d -> Date.to_string d
             | None -> Xml.text el)
      | "Designation" -> Queue.enqueue entries (entry_of el)
      | _ -> ())
  in
  let%bind entries = Queue.to_list entries |> Or_error.combine_errors in
  let version = !version in
  Snapshot.create
    ~list:Uk_sanctions_list
    ~version
    (List.map entries ~f:(fun e -> { e with version }))
;;

let parse text = parse_elements (Xml.iter_elements text ~depth:1)
let of_channel ic = parse_elements (Xml.iter_elements_channel ic ~depth:1)
