open! Core

let parse_dob s =
  match String.split (String.strip s) ~on:'/' |> List.map ~f:Int.of_string_opt with
  | [ Some 0; Some 0; Some y ] when y > 0 -> Some (Dob.Year y)
  | [ Some 0; Some m; Some y ] when m >= 1 && m <= 12 ->
    Some (Month { year = y; month = m })
  | [ Some d; Some m; Some y ] when m >= 1 && m <= 12 ->
    Option.try_with (fun () -> Dob.Day (Date.create_exn ~y ~m:(Month.of_int_exn m) ~d))
  | _ -> None
;;

let parse_date s =
  match parse_dob s with
  | Some (Day d) -> Some d
  | _ -> None
;;

(* "(1) Russia (2) Belarus" -> ["Russia"; "Belarus"]. *)
let split_numbered s =
  let s = String.strip s in
  if String.is_prefix s ~prefix:"("
  then
    String.split s ~on:'('
    |> List.filter_map ~f:(fun part ->
      match String.lsplit2 part ~on:')' with
      | Some (_, rest) when not (String.is_empty (String.strip rest)) ->
        Some (String.strip rest)
      | _ -> None)
  else if String.is_empty s
  then []
  else [ s ]
;;

let parse text =
  let open Or_error.Let_syntax in
  let rows = Csv.parse text in
  let%bind version, header, body =
    match rows with
    | ("Last Updated" :: date :: _) :: header :: body ->
      let version =
        match parse_date date with
        | Some d -> Date.to_string d
        | None -> String.strip date
      in
      Ok (version, header, body)
    | header :: body -> Ok ("unknown", header, body)
    | [] -> Or_error.error_s [%message "empty UK list file"]
  in
  let columns =
    List.mapi header ~f:(fun i h -> String.strip h, i)
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  in
  let%bind col =
    let need = [ "Name 6"; "Group ID"; "Group Type"; "Alias Type" ] in
    match List.find need ~f:(fun c -> not (Map.mem columns c)) with
    | Some c -> Or_error.error_s [%message "UK list without column" c]
    | None ->
      Ok
        (fun row name ->
          match Map.find columns name with
          | Some i -> Option.value_map (List.nth row i) ~default:"" ~f:String.strip
          | None -> "")
  in
  let groups = String.Table.create () in
  let order = Queue.create () in
  List.iter body ~f:(fun row ->
    let id = col row "Group ID" in
    if not (String.is_empty id)
    then (
      if not (Hashtbl.mem groups id) then Queue.enqueue order id;
      Hashtbl.add_multi groups ~key:id ~data:row));
  let%bind entries =
    Queue.to_list order
    |> List.map ~f:(fun id ->
      let rows = List.rev (Hashtbl.find_multi groups id) in
      let first = List.hd_exn rows in
      let party : Party_type.t =
        match col first "Group Type" with
        | "Individual" -> Person
        | "Entity" -> Entity
        | "Ship" -> Vessel
        | _ -> Unknown
      in
      let names =
        List.concat_map rows ~f:(fun row ->
          let name =
            List.map
              [ "Name 1"; "Name 2"; "Name 3"; "Name 4"; "Name 5"; "Name 6" ]
              ~f:(col row)
            |> List.filter ~f:(Fn.non String.is_empty)
            |> String.concat ~sep:" "
          in
          let kind : Entry.Name.Kind.t =
            match col row "Alias Type", col row "Alias Quality" with
            | "Primary name", _ -> Primary
            | "FKA", _ -> Former
            | _, "Low quality" -> Weak_alias
            | _ -> Alias
          in
          let non_latin = col row "Name Non-Latin Script" in
          { Entry.Name.name; kind }
          ::
          (if String.is_empty non_latin
           then []
           else [ { Entry.Name.name = non_latin; kind = Alias } ]))
      in
      let dobs = List.filter_map rows ~f:(fun r -> parse_dob (col r "DOB")) in
      let nationalities =
        List.concat_map rows ~f:(fun r -> split_numbered (col r "Nationality"))
        |> List.filter_map ~f:Country.of_name
      in
      Entry.create
        ~list:Uk_ofsi
        ~id
        ~version
        ~party
        ~names
        ~dobs
        ~nationalities
        ~programmes:(List.map rows ~f:(fun r -> col r "Regime"))
        ?listed_on:(parse_date (col first "Listed On"))
        ())
    |> Or_error.combine_errors
  in
  Snapshot.create ~list:Uk_ofsi ~version entries
;;
