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

module Reading = struct
  type t =
    | Phonetic
    | Visual
  [@@deriving sexp_of, compare, equal]
end

type folded =
  { text : string
  ; visual_differs : bool
  }

let digit_lookalike = function
  | '0' -> 'o'
  | '1' -> 'l'
  | '3' -> 'e'
  | '5' -> 's'
  | c -> c
;;

let is_word_char cp =
  Unicode_fold.is_letter_or_digit cp || Unicode_fold.is_ignorable cp || is_apostrophe cp
;;

(* Splits Unicode-folded text into words: runs of letters and digits, where invisible
   code points and apostrophes do not end a word. *)
let words folded =
  let out = ref [] in
  let buf = Buffer.create 16 in
  let flush () =
    if Buffer.length buf > 0
    then (
      out := Buffer.contents buf :: !out;
      Buffer.clear buf)
  in
  Utf8.iter folded ~f:(fun cp -> if is_word_char cp then Utf8.encode buf cp else flush ());
  flush ();
  List.rev !out
;;

(* A word looks disguised when it mixes Latin letters with Cyrillic, Greek or Armenian
   ones, when every letter in it is a non-Latin look-alike of a Latin one, or when it
   mixes letters and digits. Decided per word, so an honest Cyrillic word next to a
   disguised one is still read by transliteration. *)
let disguised_word word =
  let ascii = ref false
  and lookalike = ref false
  and all_confusable = ref true
  and digit = ref false
  and letter = ref false in
  Utf8.iter word ~f:(fun cp ->
    if Unicode_fold.is_letter_or_digit cp
    then (
      let is_digit = cp >= Char.to_int '0' && cp <= Char.to_int '9' in
      if is_digit then digit := true else letter := true;
      if cp < 0x80 && not is_digit then ascii := true;
      if Translit.is_lookalike_script cp
      then (
        lookalike := true;
        if Option.is_none (Translit.confusable cp) then all_confusable := false)));
  (!ascii && !lookalike) || (!lookalike && !all_confusable) || (!digit && !letter)
;;

(* Latin spelling of one word. Separators inside it (a letter-like symbol without a
   table entry) become spaces. *)
let latinise_word ~visual word =
  let buf = Buffer.create (String.length word) in
  let space () =
    if Buffer.length buf > 0 && Char.( <> ) (Buffer.nth buf (Buffer.length buf - 1)) ' '
    then Buffer.add_char buf ' '
  in
  let prev = ref 0 in
  Utf8.iter word ~f:(fun cp ->
    (if cp < 0x80
     then (
       let c = Char.of_int_exn cp in
       if Char.is_alphanum c
       then Buffer.add_char buf (if visual then digit_lookalike c else c)
       else if is_apostrophe cp
       then ()
       else space ())
     else if is_apostrophe cp || Unicode_fold.is_ignorable cp
     then ()
     else (
       match visual, Translit.confusable cp with
       | true, Some l -> Buffer.add_string buf l
       | _ ->
         if cp = 0x3C5 && !prev = 0x3BF
         then (* ELOT 743: omicron upsilon is "ou", not "oy". *)
           Buffer.add_char buf 'u'
         else (
           match Translit.latin cp with
           | Some l -> Buffer.add_string buf l
           | None ->
             (* A letter or digit we cannot Latinise is kept as it is, never dropped:
                deleting it would let unlisted scripts hide inside a name. *)
             if Unicode_fold.is_letter_or_digit cp then Utf8.encode buf cp else space ())));
    prev := cp);
  String.strip ~drop:(Char.equal ' ') (Buffer.contents buf)
;;

let fold_reading ~(reading : Reading.t) s =
  let ws = words (Unicode_fold.fold s) in
  let visual_differs = ref false in
  let parts =
    List.map ws ~f:(fun w ->
      let disguised = disguised_word w in
      if disguised then visual_differs := true;
      let visual =
        match reading with
        | Phonetic -> false
        | Visual -> disguised
      in
      latinise_word ~visual w)
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  { text = String.concat parts ~sep:" "; visual_differs = !visual_differs }
;;

let fold s = (fold_reading ~reading:Phonetic s).text

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

(* Roman numerals from 2 to 20 read as numbers ("Ocean Pearl VII" is "ocean pearl 7").
   Single letters are left alone: "I" or "V" is more often an initial. *)
let roman =
  String.Map.of_alist_exn
    [ "ii", "2"; "iii", "3"; "iv", "4"; "vi", "6"; "vii", "7"; "viii", "8"; "ix", "9"
    ; "xi", "11"; "xii", "12"; "xiii", "13"; "xiv", "14"; "xv", "15"; "xvi", "16"
    ; "xvii", "17"; "xviii", "18"; "xix", "19"; "xx", "20" ]
;;

let tokens_of_folded ~(party : Party_type.t) folded =
  let all =
    String.split folded ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:(fun w -> Option.value (Map.find roman w) ~default:w)
  in
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

let tokens ?(party = Party_type.Unknown) s = tokens_of_folded ~party (fold s)

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
    ; compact : string
    ; skeleton : string
    ; abjad : bool
    ; reading : Reading.t
    }
  [@@deriving sexp_of, compare, equal]

  let is_empty t = Array.is_empty t.tokens
end

let key_of_folded ~party ~reading ~abjad folded =
  let words = tokens_of_folded ~party folded in
  let tokens = List.dedup_and_sort words ~compare:String.compare in
  let skeleton =
    List.map tokens ~f:skeleton_of_token
    |> List.sort ~compare:String.compare
    |> String.concat ~sep:" "
  in
  { Key.tokens = Array.of_list tokens
  ; sorted = String.concat tokens ~sep:" "
  ; compact = String.concat words
  ; skeleton
  ; abjad
  ; reading
  }
;;

let has_abjad s =
  let abjad = ref false in
  Utf8.iter s ~f:(fun cp -> if Translit.is_abjad cp then abjad := true);
  !abjad
;;

let key ?(party = Party_type.Unknown) s =
  key_of_folded
    ~party
    ~reading:Phonetic
    ~abjad:(has_abjad s)
    (fold_reading ~reading:Phonetic s).text
;;

let keys ?(party = Party_type.Unknown) s =
  let abjad = has_abjad s in
  let phonetic = fold_reading ~reading:Phonetic s in
  let primary = key_of_folded ~party ~reading:Phonetic ~abjad phonetic.text in
  if phonetic.visual_differs
  then (
    let visual = fold_reading ~reading:Visual s in
    let second = key_of_folded ~party ~reading:Visual ~abjad visual.text in
    if String.equal second.sorted primary.sorted then [ primary ] else [ primary; second ])
  else [ primary ]
;;
