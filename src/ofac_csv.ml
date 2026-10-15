open! Core

let null s =
  let s = String.strip s in
  if String.equal s "-0-" then "" else s
;;

(* "PARAMONOV, Oleg Ivanovich" -> "Oleg Ivanovich PARAMONOV" for people. *)
let person_name name =
  match String.lsplit2 name ~on:',' with
  | Some (last, given) when not (String.is_empty (String.strip given)) ->
    String.strip given ^ " " ^ String.strip last
  | _ -> name
;;

let months =
  [ "jan", 1; "feb", 2; "mar", 3; "apr", 4; "may", 5; "jun", 6; "jul", 7; "aug", 8
  ; "sep", 9; "oct", 10; "nov", 11; "dec", 12 ]
;;

let parse_dob s =
  let s = String.lowercase (String.strip s) in
  let year y =
    match Int.of_string_opt y with
    | Some y when y >= 1000 && y <= 9999 -> Some y
    | _ -> None
  in
  match String.split s ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) with
  | [ "circa"; y ] -> Option.map (year y) ~f:(fun y -> Dob.Years { from = y - 1; until = y + 1 })
  | [ a; "to"; b ] ->
    (match year a, year b with
     | Some from, Some until when from <= until -> Some (Years { from; until })
     | _ -> None)
  | [ y ] -> Option.map (year y) ~f:(fun y -> Dob.Year y)
  | [ m; y ] ->
    (match List.Assoc.find months (String.prefix m 3) ~equal:String.equal, year y with
     | Some month, Some year -> Some (Month { year; month })
     | _ -> None)
  | [ d; m; y ] ->
    (match
       Int.of_string_opt d, List.Assoc.find months (String.prefix m 3) ~equal:String.equal, year y
     with
     | Some d, Some m, Some y ->
       Option.try_with (fun () -> Dob.Day (Date.create_exn ~y ~m:(Month.of_int_exn m) ~d))
     | _ -> None)
  | _ -> None
;;

let quoted_aliases remark =
  (* a.k.a. 'X'; f.k.a. 'Y' *)
  let out = ref [] in
  List.iter [ "a.k.a. '", Entry.Name.Kind.Weak_alias; "f.k.a. '", Former ] ~f:(fun (prefix, kind) ->
    let rec go pos =
      match String.substr_index remark ~pos ~pattern:prefix with
      | None -> ()
      | Some i ->
        let start = i + String.length prefix in
        (match String.index_from remark start '\'' with
         | Some stop ->
           out := { Entry.Name.name = String.sub remark ~pos:start ~len:(stop - start); kind } :: !out;
           go (stop + 1)
         | None -> ())
    in
    go 0);
  List.rev !out
;;

let parse_remarks remarks =
  let items = String.split remarks ~on:';' |> List.map ~f:String.strip in
  let dobs, nats =
    List.fold items ~init:([], []) ~f:(fun (dobs, nats) item ->
      let item = String.chop_suffix_if_exists item ~suffix:"." in
      let after prefix = String.chop_prefix item ~prefix in
      match after "DOB ", after "alt. DOB ", after "nationality ", after "citizen " with
      | Some d, _, _, _ | _, Some d, _, _ ->
        (match parse_dob d with
         | Some d -> d :: dobs, nats
         | None -> dobs, nats)
      | _, _, Some c, _ | _, _, _, Some c ->
        (match Country.of_name c with
         | Some c -> dobs, c :: nats
         | None -> dobs, nats)
      | None, None, None, None -> dobs, nats)
  in
  List.rev dobs, List.rev nats, quoted_aliases remarks
;;

let party_of_type t : Party_type.t =
  match String.lowercase (null t) with
  | "individual" -> Person
  | "vessel" -> Vessel
  | "aircraft" -> Aircraft
  | _ -> Entity
;;

let parse ~version ~sdn ?alt () =
  let open Or_error.Let_syntax in
  let aliases = String.Table.create () in
  (match alt with
   | None -> ()
   | Some alt ->
     List.iter (Csv.parse alt) ~f:(function
       | ent :: _alt_num :: kind :: name :: _ ->
         let kind : Entry.Name.Kind.t =
           match String.lowercase (null kind) with
           | "fka" -> Former
           | _ -> Alias
         in
         Hashtbl.add_multi aliases ~key:(String.strip ent) ~data:(null name, kind)
       | _ -> ()));
  let%bind entries =
    Csv.parse sdn
    |> List.filter ~f:(fun row -> List.length row >= 12)
    |> List.map ~f:(fun row ->
      let field i = null (List.nth_exn row i) in
      let id = field 0 in
      let party = party_of_type (field 2) in
      let fix name = if Party_type.equal party Person then person_name name else name in
      let dobs, nationalities, weak = parse_remarks (field 11) in
      let alts =
        Hashtbl.find_multi aliases id
        |> List.rev
        |> List.map ~f:(fun (name, kind) -> { Entry.Name.name = fix name; kind })
      in
      let programmes =
        String.substr_replace_all (field 3) ~pattern:"] [" ~with_:"|"
        |> String.split ~on:'|'
        |> List.map ~f:(fun p -> String.strip ~drop:(fun c -> Char.equal c '[' || Char.equal c ']') p)
      in
      Entry.create
        ~list:Ofac_sdn
        ~id
        ~version
        ~party
        ~names:({ Entry.Name.name = fix (field 1); kind = Primary } :: (alts @ weak))
        ~dobs
        ~nationalities
        ~programmes
        ())
    |> Or_error.combine_errors
  in
  Snapshot.create ~list:Ofac_sdn ~version entries
;;
