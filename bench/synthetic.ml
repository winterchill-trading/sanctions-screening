open! Core
open Sanctions_screening

(* A deterministic synthetic list: invented names from syllables, a mix of people (with
   Cyrillic and Arabic-script aliases), companies and vessels, about 2.5 names per entry
   like the real lists. *)

let syllables =
  [| "ka"
   ; "lo"
   ; "mir"
   ; "ser"
   ; "va"
   ; "dan"
   ; "tor"
   ; "ni"
   ; "ra"
   ; "bek"
   ; "zo"
   ; "lin"
   ; "har"
   ; "mo"
   ; "ste"
   ; "vik"
   ; "an"
   ; "drei"
   ; "go"
   ; "rov"
   ; "sha"
   ; "hid"
   ; "bar"
   ; "kel"
   ; "ume"
   ; "fa"
   ; "rid"
   ; "qa"
   ; "sim"
   ; "yus"
   ; "uf"
   ; "kar"
   ; "pet"
   ; "len"
   ; "olt"
   ; "ivo"
   ; "nas"
  |]
;;

let word rng n =
  String.concat
    (List.init n ~f:(fun _ -> syllables.(Random.State.int rng (Array.length syllables))))
  |> String.capitalize
;;

let cyrillic_of_latin w =
  (* A rough reverse transliteration, enough to give the index non-Latin names. *)
  String.concat_map (String.lowercase w) ~f:(function
    | 'a' -> "а"
    | 'b' -> "б"
    | 'd' -> "д"
    | 'e' -> "е"
    | 'f' -> "ф"
    | 'g' -> "г"
    | 'h' -> "х"
    | 'i' -> "и"
    | 'k' -> "к"
    | 'l' -> "л"
    | 'm' -> "м"
    | 'n' -> "н"
    | 'o' -> "о"
    | 'p' -> "п"
    | 'q' -> "к"
    | 'r' -> "р"
    | 's' -> "с"
    | 't' -> "т"
    | 'u' -> "у"
    | 'v' -> "в"
    | 'y' -> "й"
    | 'z' -> "з"
    | c -> String.of_char c)
;;

let entries ~n =
  let rng = Random.State.make [| 42 |] in
  List.init n ~f:(fun i ->
    let kind = Random.State.int rng 10 in
    let party : Party_type.t =
      if kind < 6 then Person else if kind < 9 then Entity else Vessel
    in
    let names =
      match party with
      | Person ->
        let first = word rng 2
        and middle = word rng 3
        and last = word rng 3 in
        [ { Entry.Name.name = sprintf "%s %s %s" first middle last; kind = Primary }
        ; { name = sprintf "%s %s" first last; kind = Alias }
        ; { name = cyrillic_of_latin (sprintf "%s %s" first last); kind = Alias }
        ]
      | _ ->
        let base = sprintf "%s %s" (word rng 3) (word rng 2) in
        [ { Entry.Name.name = base ^ " Ltd"; kind = Primary }
        ; { name = base ^ " Trading"; kind = Alias }
        ]
    in
    Entry.create
      ~list:(Other "synthetic")
      ~id:(sprintf "S-%05d" i)
      ~version:"bench"
      ~party
      ~names
      ~dobs:(if Party_type.equal party Person then [ Year (1940 + (i mod 60)) ] else [])
      ()
    |> ok_exn)
;;

let lists ~n =
  Lists.create
    [ Snapshot.create ~list:(Other "synthetic") ~version:"bench" (entries ~n) |> ok_exn ]
  |> ok_exn
;;
