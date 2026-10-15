open! Core

let is_apostrophe cp =
  cp = Char.to_int '\''
  || cp = Char.to_int '`'
  || cp = 0x2019
  || cp = 0x2018
  || cp = 0x2BC
  || cp = 0x2BB
  || cp = 0x2BF
  || cp = 0x2BE
;;

let fold s =
  let folded = Unicode_fold.fold s in
  let buf = Buffer.create (String.length folded) in
  let space () =
    if Buffer.length buf > 0 && Char.( <> ) (Buffer.nth buf (Buffer.length buf - 1)) ' '
    then Buffer.add_char buf ' '
  in
  let prev = ref 0 in
  Utf8.iter folded ~f:(fun cp ->
    (if cp < 0x80
     then (
       let c = Char.of_int_exn cp in
       if Char.is_alphanum c
       then Buffer.add_char buf c
       else if is_apostrophe cp
       then ()
       else space ())
     else if is_apostrophe cp
     then ()
     else if cp = 0x3C5 && !prev = 0x3BF
     then (* ELOT 743: omicron upsilon is "ou", not "oy". *)
       Buffer.add_char buf 'u'
     else (
       match Translit.latin cp with
       | Some l -> Buffer.add_string buf l
       | None ->
         (* Letters of scripts without a table (CJK, Hangul, ...) are kept as they are
            so that a name still matches its own spelling; anything else separates. *)
         if cp >= 0x2E80 then Utf8.encode buf cp else space ()));
    prev := cp);
  String.rstrip ~drop:(Char.equal ' ') (Buffer.contents buf)
;;

let honorifics =
  [ "mr"
  ; "mrs"
  ; "ms"
  ; "miss"
  ; "mx"
  ; "dr"
  ; "prof"
  ; "sir"
  ; "dame"
  ; "lord"
  ; "lady"
  ; "rev"
  ; "herr"
  ; "frau"
  ; "mme"
  ; "mlle"
  ; "sr"
  ; "sra"
  ; "srta"
  ; "dott"
  ; "ing"
  ; "sheikh"
  ; "shaikh"
  ; "sheik"
  ; "haji"
  ; "hajji"
  ; "hadji"
  ; "mullah"
  ; "maulana"
  ; "imam"
  ; "sayyid"
  ; "gen"
  ; "general"
  ; "col"
  ; "colonel"
  ; "maj"
  ; "major"
  ; "capt"
  ; "captain"
  ; "lt"
  ; "adm"
  ; "admiral"
  ; "brig"
  ; "gospodin"
  ; "gospozha"
  ]
;;

let legal_forms =
  [ "ltd"
  ; "limited"
  ; "llc"
  ; "llp"
  ; "lp"
  ; "plc"
  ; "inc"
  ; "incorporated"
  ; "corp"
  ; "corporation"
  ; "co"
  ; "company"
  ; "gmbh"
  ; "mbh"
  ; "ag"
  ; "kg"
  ; "kgaa"
  ; "se"
  ; "sa"
  ; "sarl"
  ; "sas"
  ; "sasu"
  ; "srl"
  ; "spa"
  ; "sl"
  ; "bv"
  ; "nv"
  ; "ab"
  ; "as"
  ; "asa"
  ; "aps"
  ; "oy"
  ; "oyj"
  ; "ooo"
  ; "oao"
  ; "zao"
  ; "pao"
  ; "ao"
  ; "ojsc"
  ; "cjsc"
  ; "pjsc"
  ; "jsc"
  ; "tov"
  ; "fze"
  ; "fzco"
  ; "fzc"
  ; "fzllc"
  ; "dmcc"
  ; "pte"
  ; "pty"
  ; "pvt"
  ; "bhd"
  ; "sdn"
  ; "kk"
  ; "doo"
  ; "dd"
  ; "sp"
  ; "zoo"
  ; "sro"
  ; "as"
  ; "kft"
  ; "zrt"
  ; "ltda"
  ; "cia"
  ; "sac"
  ; "eirl"
  ]
;;

let joining_words = [ "the"; "and"; "und"; "et"; "y" ]
let vessel_prefixes = [ "mv"; "mt"; "ms"; "ss"; "mts"; "fv" ]
let honorific_set = String.Set.of_list honorifics
let entity_set = String.Set.of_list (legal_forms @ joining_words)
let vessel_set = String.Set.of_list vessel_prefixes

let tokens ?(party = Party_type.Unknown) s =
  let all = String.split (fold s) ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) in
  let all =
    (* "M/V" and "M/T" fold to two one-letter words. *)
    match party, all with
    | Vessel, "m" :: ("v" | "t") :: (_ :: _ as rest) -> rest
    | _ -> all
  in
  let drop w =
    match party with
    | Person -> Set.mem honorific_set w
    | Entity -> Set.mem entity_set w
    | Vessel -> Set.mem vessel_set w
    | Aircraft -> false
    | Unknown -> Set.mem honorific_set w || Set.mem entity_set w
  in
  match List.filter all ~f:(Fn.non drop) with
  | [] -> all
  | kept -> kept
;;

let is_vowel c =
  match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> true
  | _ -> false
;;

let skeleton_of_token t =
  let buf = Buffer.create (String.length t) in
  String.iter t ~f:(fun c ->
    if not (is_vowel c)
    then
      if Buffer.length buf = 0 || Char.( <> ) (Buffer.nth buf (Buffer.length buf - 1)) c
      then Buffer.add_char buf c);
  (* A token made only of vowels keeps its first letter so it is not lost. *)
  if Buffer.length buf = 0 && not (String.is_empty t)
  then String.prefix t 1
  else Buffer.contents buf
;;

module Key = struct
  type t =
    { tokens : string array
    ; sorted : string
    ; skeleton : string
    ; abjad : bool
    }
  [@@deriving sexp_of, compare, equal]

  let is_empty t = Array.is_empty t.tokens
end

let key ?party s =
  let tokens = tokens ?party s |> List.dedup_and_sort ~compare:String.compare in
  let abjad = ref false in
  Utf8.iter s ~f:(fun cp -> if Translit.is_abjad cp then abjad := true);
  let skeleton =
    List.map tokens ~f:skeleton_of_token
    |> List.sort ~compare:String.compare
    |> String.concat ~sep:" "
  in
  { Key.tokens = Array.of_list tokens
  ; sorted = String.concat tokens ~sep:" "
  ; skeleton
  ; abjad = !abjad
  }
;;
