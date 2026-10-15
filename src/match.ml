open! Core

module Scratch = struct
  type t =
    { flags_a : Bytes.t
    ; flags_b : Bytes.t
    ; row0 : int array
    ; row1 : int array
    ; row2 : int array
    ; used : Bytes.t
    }

  let max_length = 255

  let create () =
    { flags_a = Bytes.make (max_length + 1) '\000'
    ; flags_b = Bytes.make (max_length + 1) '\000'
    ; row0 = Array.create ~len:(max_length + 2) 0
    ; row1 = Array.create ~len:(max_length + 2) 0
    ; row2 = Array.create ~len:(max_length + 2) 0
    ; used = Bytes.make (max_length + 1) '\000'
    }
  ;;
end

let[@inline] clamp_len s = Int.min (String.length s) Scratch.max_length

(* Integer division rounded to nearest, for non-negative operands. *)
let[@inline] div_round a b = ((2 * a) + b) / (2 * b)

let[@zero_alloc] jaro_winkler (scratch : Scratch.t) a b =
  let la = clamp_len a
  and lb = clamp_len b in
  if la = 0 && lb = 0
  then 1000
  else if la = 0 || lb = 0
  then 0
  else (
    let fa = scratch.flags_a
    and fb = scratch.flags_b in
    for i = 0 to la - 1 do
      Bytes.unsafe_set fa i '\000'
    done;
    for i = 0 to lb - 1 do
      Bytes.unsafe_set fb i '\000'
    done;
    let window = Int.max 0 ((Int.max la lb / 2) - 1) in
    let matches = ref 0 in
    for i = 0 to la - 1 do
      let lo = Int.max 0 (i - window)
      and hi = Int.min (lb - 1) (i + window) in
      let j = ref lo in
      while !j <= hi do
        if Char.equal (Bytes.unsafe_get fb !j) '\000'
           && Char.equal (String.unsafe_get a i) (String.unsafe_get b !j)
        then (
          Bytes.unsafe_set fa i '\001';
          Bytes.unsafe_set fb !j '\001';
          incr matches;
          j := hi + 1)
        else incr j
      done
    done;
    let m = !matches in
    if m = 0
    then 0
    else (
      let k = ref 0
      and half_transpositions = ref 0 in
      for i = 0 to la - 1 do
        if Char.equal (Bytes.unsafe_get fa i) '\001'
        then (
          while Char.equal (Bytes.unsafe_get fb !k) '\000' do
            incr k
          done;
          if not (Char.equal (String.unsafe_get a i) (String.unsafe_get b !k))
          then incr half_transpositions;
          incr k)
      done;
      let t = !half_transpositions / 2 in
      (* jaro = (m/la + m/lb + (m-t)/m) / 3, scaled to 1000 with one rounding. *)
      let num = 1000 * ((m * lb * m) + (m * la * m) + ((m - t) * la * lb)) in
      let den = 3 * la * lb * m in
      let jaro = div_round num den in
      if jaro <= 700
      then jaro
      else (
        let prefix = ref 0 in
        let limit = Int.min 4 (Int.min la lb) in
        while !prefix < limit && Char.equal (String.unsafe_get a !prefix) (String.unsafe_get b !prefix) do
          incr prefix
        done;
        jaro + div_round (!prefix * (1000 - jaro)) 10)))
;;

let[@zero_alloc] damerau_levenshtein (scratch : Scratch.t) a b =
  let la = clamp_len a
  and lb = clamp_len b in
  if la = 0
  then lb
  else if lb = 0
  then la
  else (
    (* Rows i-2, i-1 and i of the optimal string alignment table, rotated by index. *)
    let r0 = scratch.row0
    and r1 = scratch.row1
    and r2 = scratch.row2 in
    for j = 0 to lb do
      Array.unsafe_set r1 j j
    done;
    let prev2 = ref r0
    and prev = ref r1
    and cur = ref r2 in
    for i = 1 to la do
      let c = !cur
      and p = !prev
      and pp = !prev2 in
      Array.unsafe_set c 0 i;
      let ca = String.unsafe_get a (i - 1) in
      for j = 1 to lb do
        let cb = String.unsafe_get b (j - 1) in
        let cost = if Char.equal ca cb then 0 else 1 in
        let v =
          Int.min
            (Int.min (Array.unsafe_get p j + 1) (Array.unsafe_get c (j - 1) + 1))
            (Array.unsafe_get p (j - 1) + cost)
        in
        let v =
          if i > 1
             && j > 1
             && Char.equal ca (String.unsafe_get b (j - 2))
             && Char.equal (String.unsafe_get a (i - 2)) cb
          then Int.min v (Array.unsafe_get pp (j - 2) + 1)
          else v
        in
        Array.unsafe_set c j v
      done;
      prev2 := p;
      prev := c;
      cur := pp
    done;
    Array.unsafe_get !prev lb)
;;

let[@zero_alloc] edit_similarity scratch a b =
  let la = clamp_len a
  and lb = clamp_len b in
  let longer = Int.max la lb in
  if longer = 0
  then 1000
  else 1000 - div_round (1000 * damerau_levenshtein scratch a b) longer
;;

let[@inline] soundex_digit c =
  match c with
  | 'b' | 'f' | 'p' | 'v' -> 1
  | 'c' | 'g' | 'j' | 'k' | 'q' | 's' | 'x' | 'z' -> 2
  | 'd' | 't' -> 3
  | 'l' -> 4
  | 'm' | 'n' -> 5
  | 'r' -> 6
  | 'h' | 'w' -> -1 (* transparent: do not separate equal codes *)
  | _ -> 0 (* vowels and others: separate *)
;;

let[@zero_alloc] soundex w =
  let n = String.length w in
  let first = ref (-1) in
  let i = ref 0 in
  while !first < 0 && !i < n do
    let c = String.unsafe_get w !i in
    if Char.is_lowercase c then first := !i else incr i
  done;
  if !first < 0
  then 0
  else (
    let f = String.unsafe_get w !first in
    let code = ref 0
    and digits = ref 0
    and last = ref (soundex_digit f) in
    let k = ref (!first + 1) in
    while !digits < 3 && !k < n do
      let d = soundex_digit (String.unsafe_get w !k) in
      if d > 0 && d <> !last
      then (
        code := (!code * 10) + d;
        incr digits);
      if d >= 0 then last := d;
      incr k
    done;
    while !digits < 3 do
      code := !code * 10;
      incr digits
    done;
    (Char.to_int f * 1000) + !code)
;;

let soundex_to_string code =
  if code = 0
  then ""
  else
    sprintf
      "%c%03d"
      (Char.uppercase (Char.of_int_exn (code / 1000)))
      (code mod 1000)
;;

let[@zero_alloc] matched_tokens (scratch : Scratch.t) ~token_threshold a b =
  let short, long = if Array.length a <= Array.length b then a, b else b, a in
  let nl = Int.min (Array.length long) Scratch.max_length in
  let used = scratch.used in
  for i = 0 to nl - 1 do
    Bytes.unsafe_set used i '\000'
  done;
  let pairs = ref 0 in
  for i = 0 to Int.min (Array.length short) Scratch.max_length - 1 do
    let t = Array.unsafe_get short i in
    let best = ref (-1)
    and best_score = ref (-1) in
    for j = 0 to nl - 1 do
      if Char.equal (Bytes.unsafe_get used j) '\000'
      then (
        let s = jaro_winkler scratch t (Array.unsafe_get long j) in
        if s >= token_threshold && s > !best_score
        then (
          best := j;
          best_score := s))
    done;
    if !best >= 0
    then (
      Bytes.unsafe_set used !best '\001';
      incr pairs)
  done;
  !pairs
;;

let[@zero_alloc] common_codes (a : int array) (b : int array) =
  let i = ref 0
  and j = ref 0
  and n = ref 0 in
  while !i < Array.length a && !j < Array.length b do
    let x = Array.unsafe_get a !i
    and y = Array.unsafe_get b !j in
    if x = y
    then (
      incr n;
      incr i;
      incr j)
    else if x < y
    then incr i
    else incr j
  done;
  !n
;;

module Components = struct
  type t =
    { jaro_winkler : int
    ; edit : int
    ; compact : int
    ; token_set : int
    ; token_cover : int
    ; phonetic : int
    ; skeleton : int
    }
  [@@deriving sexp, compare, equal, bin_io]
end

module Weights = struct
  type t =
    { token_set_share : int
    ; phonetic_cap : int
    ; skeleton_cap : int
    ; token_threshold : int
    }
  [@@deriving sexp, compare, equal, bin_io]

  let default =
    { token_set_share = 600; phonetic_cap = 880; skeleton_cap = 930; token_threshold = 900 }
  ;;
end

module Name_score = struct
  type t =
    { score : int
    ; best : string
    ; components : Components.t
    }
  [@@deriving sexp, compare, equal, bin_io]
end

module Prepared = struct
  type t =
    { key : Normalize.Key.t
    ; codes : int array
    }

  let of_key (key : Normalize.Key.t) =
    let codes = Array.map key.tokens ~f:soundex in
    Array.sort codes ~compare:Int.compare;
    { key; codes }
  ;;
end

(* The components, computed without allocating; [explain] boxes them for a hit. *)
let[@inline] components scratch (w : Weights.t) (a : Prepared.t) (b : Prepared.t) =
  let ka = a.key
  and kb = b.key in
  let jw = jaro_winkler scratch ka.sorted kb.sorted in
  let ed = edit_similarity scratch ka.sorted kb.sorted in
  let compact =
    Int.max
      (jaro_winkler scratch ka.compact kb.compact)
      (edit_similarity scratch ka.compact kb.compact)
  in
  let na = Array.length ka.tokens
  and nb = Array.length kb.tokens in
  let shorter = Int.min na nb
  and longer = Int.max na nb in
  let pairs =
    matched_tokens scratch ~token_threshold:w.token_threshold ka.tokens kb.tokens
  in
  let token_set = if shorter = 0 then 0 else div_round (1000 * pairs) shorter in
  let token_cover = if longer = 0 then 0 else div_round (1000 * pairs) longer in
  let phonetic =
    if longer = 0 then 0 else div_round (1000 * common_codes a.codes b.codes) longer
  in
  let skeleton =
    if ka.abjad || kb.abjad then jaro_winkler scratch ka.skeleton kb.skeleton else 0
  in
  { Components.jaro_winkler = jw
  ; edit = ed
  ; compact
  ; token_set
  ; token_cover
  ; phonetic
  ; skeleton
  }
;;

let best_of (w : Weights.t) (c : Components.t) =
  let token =
    div_round
      ((c.token_set * w.token_set_share) + (c.token_cover * (1000 - w.token_set_share)))
      1000
  in
  let candidates =
    [ "jaro_winkler", c.jaro_winkler
    ; "edit", c.edit
    ; "compact", c.compact
    ; "tokens", token
    ; "phonetic", Int.min c.phonetic w.phonetic_cap
    ; "skeleton", Int.min c.skeleton w.skeleton_cap
    ]
  in
  (* First maximum wins, so ties name the simplest component. *)
  List.fold candidates ~init:("none", -1) ~f:(fun (bn, bs) (n, s) ->
    if s > bs then n, s else bn, bs)
;;

(* Same maximum as [best_of (components ...)], kept in locals so the hot path does not
   allocate; [test/test_match.ml] checks the two agree. *)
let[@zero_alloc] score scratch (w : Weights.t) (a : Prepared.t) (b : Prepared.t) =
  let ka = a.key
  and kb = b.key in
  let best = ref (jaro_winkler scratch ka.sorted kb.sorted) in
  let keep s = if s > !best then best := s in
  keep (edit_similarity scratch ka.sorted kb.sorted);
  keep (jaro_winkler scratch ka.compact kb.compact);
  keep (edit_similarity scratch ka.compact kb.compact);
  let na = Array.length ka.tokens
  and nb = Array.length kb.tokens in
  let shorter = Int.min na nb
  and longer = Int.max na nb in
  if longer > 0
  then (
    let pairs =
      matched_tokens scratch ~token_threshold:w.token_threshold ka.tokens kb.tokens
    in
    let token_set = if shorter = 0 then 0 else div_round (1000 * pairs) shorter in
    let token_cover = div_round (1000 * pairs) longer in
    keep
      (div_round
         ((token_set * w.token_set_share) + (token_cover * (1000 - w.token_set_share)))
         1000);
    keep
      (Int.min
         w.phonetic_cap
         (div_round (1000 * common_codes a.codes b.codes) longer)));
  if ka.abjad || kb.abjad
  then keep (Int.min w.skeleton_cap (jaro_winkler scratch ka.skeleton kb.skeleton));
  !best
;;

let explain scratch w a b =
  let components = components scratch w a b in
  let best, score = best_of w components in
  { Name_score.score; best; components }
;;
