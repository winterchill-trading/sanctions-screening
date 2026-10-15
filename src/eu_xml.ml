open! Core

let parse text =
  let open Or_error.Let_syntax in
  (* The root's attributes need the root element; read them from the opening tag
     instead of building the whole document. *)
  let version =
    match String.substr_index text ~pattern:"generationDate=\"" with
    | Some i -> String.sub text ~pos:(i + 16) ~len:10
    | None -> "unknown"
  in
  let entities = Queue.create () in
  let%bind () =
    Xml.iter_elements text ~depth:1 ~f:(fun el ->
      if String.equal el.name "sanctionEntity" then Queue.enqueue entities el)
  in
  let%bind entries =
    Queue.to_list entities
    |> List.map ~f:(fun el ->
      let party : Party_type.t =
        match Option.map (Xml.child el "subjectType") ~f:(fun s -> Xml.attr_or_empty s "classificationCode") with
        | Some "P" -> Person
        | Some "E" -> Entity
        | _ -> Unknown
      in
      let names =
        Xml.children el "nameAlias"
        |> List.mapi ~f:(fun i a ->
          let whole = Xml.attr_or_empty a "wholeName" in
          let name =
            if String.is_empty (String.strip whole)
            then
              [ "firstName"; "middleName"; "lastName" ]
              |> List.map ~f:(Xml.attr_or_empty a)
              |> List.filter ~f:(Fn.non String.is_empty)
              |> String.concat ~sep:" "
            else whole
          in
          let kind : Entry.Name.Kind.t =
            if i = 0
            then Primary
            else if String.equal (Xml.attr_or_empty a "strong") "false"
            then Weak_alias
            else Alias
          in
          { Entry.Name.name; kind })
      in
      let dobs =
        Xml.children el "birthdate"
        |> List.filter_map ~f:(fun b ->
          let circa = String.equal (Xml.attr_or_empty b "circa") "true" in
          match Xml.attr_or_empty b "birthdate", Int.of_string_opt (Xml.attr_or_empty b "year") with
          | d, _ when (not (String.is_empty d)) && not circa ->
            Option.try_with (fun () -> Dob.Day (Date.of_string d))
          | _, Some y when circa -> Some (Dob.Years { from = y - 1; until = y + 1 })
          | _, Some y -> Some (Year y)
          | _ -> None)
      in
      let regulations = Xml.children el "regulation" in
      Entry.create
        ~list:Eu_consolidated
        ~id:(Xml.attr_or_empty el "logicalId")
        ~version
        ~party
        ~names
        ~dobs
        ~nationalities:
          (List.map (Xml.children el "citizenship") ~f:(fun c -> Xml.attr_or_empty c "countryIso2Code"))
        ~programmes:(List.map regulations ~f:(fun r -> Xml.attr_or_empty r "programme"))
        ?listed_on:
          (List.filter_map regulations ~f:(fun r ->
             Option.try_with (fun () -> Date.of_string (Xml.attr_or_empty r "publicationDate")))
           |> List.min_elt ~compare:Date.compare)
        ())
    |> Or_error.combine_errors
  in
  Snapshot.create ~list:Eu_consolidated ~version entries
;;
