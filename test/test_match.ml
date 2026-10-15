open! Core
open Sanctions_screening

let s = Match.Scratch.create ()

let%expect_test "Jaro-Winkler reference values" =
  List.iter
    [ "martha", "marhta"
    ; "dwayne", "duane"
    ; "dixon", "dicksonx"
    ; "jellyfish", "smellyfish"
    ; "abc", "abc"
    ; "abc", "xyz"
    ; "", ""
    ; "a", ""
    ]
    ~f:(fun (a, b) -> printf "%-10s %-10s %4d\n" a b (Match.jaro_winkler s a b));
  [%expect
    {|
    martha     marhta      961
    dwayne     duane       840
    dixon      dicksonx    814
    jellyfish  smellyfish  896
    abc        abc        1000
    abc        xyz           0
                          1000
    a                        0
    |}]
;;

let%expect_test "Damerau-Levenshtein (optimal string alignment)" =
  List.iter
    [ "ca", "ac"
    ; "kitten", "sitting"
    ; "ivanov", "ivnaov"
    ; "abc", ""
    ; "paramonov", "paramonov"
    ; "ca", "abc"
    ]
    ~f:(fun (a, b) ->
      printf
        "%-10s %-10s %d %4d\n"
        a
        b
        (Match.damerau_levenshtein s a b)
        (Match.edit_similarity s a b));
  [%expect
    {|
    ca         ac         1  500
    kitten     sitting    3  571
    ivanov     ivnaov     1  833
    abc                   3    0
    paramonov  paramonov  0 1000
    ca         abc        3    0
    |}]
;;

let%expect_test "Soundex" =
  List.iter
    [ "robert"; "rupert"; "rubin"; "ashcraft"; "tymczak"; "pfister"; "honeyman"; "1234" ]
    ~f:(fun w -> printf "%-9s %s\n" w (Match.soundex_to_string (Match.soundex w)));
  [%expect
    {|
    robert    R163
    rupert    R163
    rubin     R150
    ashcraft  A261
    tymczak   T522
    pfister   P236
    honeyman  H555
    1234
    |}]
;;

let prep ?party n = Match.Prepared.of_key (Normalize.key ?party n)

let%expect_test "explained name scores" =
  let show a b =
    let e = Match.explain s Match.Weights.default (prep a) (prep b) in
    printf "%s | %s\n" a b;
    print_s [%sexp (e : Match.Name_score.t)]
  in
  show "John Smith" "SMITH, John";
  show "John Smith" "John William Smith";
  show "Jon Smyth" "John Smith";
  show "Muhammad Abdullah" "محمد عبد الله";
  show "Northwind Trading Ltd" "Northwind Trading";
  show "Robert Brown" "Rupert Green";
  [%expect
    {|
    John Smith | SMITH, John
    ((score 1000) (best edit)
     (components
      ((jaro_winkler 1000) (edit 1000) (compact 111) (token_set 1000)
       (token_cover 1000) (phonetic 1000) (skeleton 0))))
    John Smith | John William Smith
    ((score 867) (best tokens)
     (components
      ((jaro_winkler 911) (edit 556) (compact 562) (token_set 1000)
       (token_cover 667) (phonetic 667) (skeleton 0))))
    Jon Smyth | John Smith
    ((score 914) (best tokens)
     (components
      ((jaro_winkler 917) (edit 800) (compact 778) (token_set 914)
       (token_cover 914) (phonetic 1000) (skeleton 0))))
    Muhammad Abdullah | محمد عبد الله
    ((score 930) (best skeleton)
     (components
      ((jaro_winkler 788) (edit 412) (compact 562) (token_set 0) (token_cover 0)
       (phonetic 0) (skeleton 944))))
    Northwind Trading Ltd | Northwind Trading
    ((score 1000) (best edit)
     (components
      ((jaro_winkler 1000) (edit 1000) (compact 1000) (token_set 1000)
       (token_cover 1000) (phonetic 1000) (skeleton 0))))
    Robert Brown | Rupert Green
    ((score 583) (best edit)
     (components
      ((jaro_winkler 722) (edit 583) (compact 545) (token_set 0) (token_cover 0)
       (phonetic 500) (skeleton 0))))
    |}]
;;

(* A small alphabet makes near-misses and repeated letters common. *)
let gen_word =
  let open Quickcheck.Generator.Let_syntax in
  let%bind n = Int.gen_incl 0 12 in
  String.gen_with_length n (Char.gen_uniform_inclusive 'a' 'f')
;;

let%expect_test "symmetry and bounds" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 gen_word gen_word)
    ~sexp_of:[%sexp_of: string * string]
    ~f:(fun (a, b) ->
      let jw = Match.jaro_winkler s a b in
      [%test_result: int] (Match.jaro_winkler s b a) ~expect:jw;
      [%test_result: int]
        (Match.damerau_levenshtein s b a)
        ~expect:(Match.damerau_levenshtein s a b);
      assert (jw >= 0 && jw <= 1000);
      if String.equal a b then [%test_result: int] jw ~expect:1000);
  [%expect {| |}]
;;

let gen_name =
  let open Quickcheck.Generator.Let_syntax in
  let%bind words = List.gen_with_length 3 gen_word in
  let%bind k = Int.gen_incl 1 3 in
  return (String.concat ~sep:" " (List.take words k))
;;

let%expect_test "name score is symmetric and score agrees with explain" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 gen_name gen_name)
    ~sexp_of:[%sexp_of: string * string]
    ~f:(fun (a, b) ->
      let pa = prep a
      and pb = prep b in
      let w = Match.Weights.default in
      let ab = Match.score s w pa pb in
      [%test_result: int] (Match.score s w pb pa) ~expect:ab;
      [%test_result: int] (Match.explain s w pa pb).score ~expect:ab);
  [%expect {| |}]
;;

let%expect_test "score allocates nothing" =
  let a = prep "Oleg Paramonov"
  and b = prep "Paramonov Oleg Ivanovich" in
  let before = Gc.minor_words () in
  let r = ref 0 in
  for _ = 1 to 100 do
    r := !r + Match.score s Match.Weights.default a b
  done;
  let after = Gc.minor_words () in
  printf "words per call: %d\n" ((after - before) / 100);
  [%expect {| words per call: 0 |}]
;;
