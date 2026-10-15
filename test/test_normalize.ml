open! Core
open Sanctions_screening

let%expect_test "fold" =
  List.iter
    ~f:(fun s -> print_endline (Normalize.fold s))
    [ "  Dr. José-María  O'Neil "
    ; "ООО «Северный Ветер»"
    ; "Παπαδόπουλος"
    ; "عبد الله"
    ; "ZÜRICH   Ärzte-GmbH & Co. KG"
    ; "王小明"
    ];
  [%expect
    {|
    dr jose maria oneil
    ooo severnyi veter
    papadopoulos
    bd allh
    zurich arzte gmbh co kg
    王小明
    |}]
;;

let%expect_test "tokens drop words by party type" =
  let show party s =
    print_s [%sexp (Normalize.tokens ~party s : string list), (party : Party_type.t)]
  in
  show Person "Mr. John Smith";
  show Entity "The Northwind Trading Co. Ltd";
  show Entity "ООО Ромашка";
  show Vessel "M/V Silver Heron";
  show Unknown "Dr. Northwind GmbH";
  show Entity "Ltd Co";
  [%expect
    {|
    ((john smith) Person)
    ((northwind trading) Entity)
    ((romashka) Entity)
    ((silver heron) Vessel)
    ((northwind) Unknown)
    ((ltd co) Entity)
    |}]
;;

let%expect_test "key sorts, dedups and builds the skeleton" =
  print_s [%sexp (Normalize.key ~party:Person "SMITH, John John" : Normalize.Key.t)];
  print_s [%sexp (Normalize.key ~party:Person "محمد عبد الله" : Normalize.Key.t)];
  print_s [%sexp (Normalize.key ~party:Person "Muhammad Abdullah" : Normalize.Key.t)];
  [%expect
    {|
    ((tokens (john smith)) (sorted "john smith") (compact smithjohnjohn)
     (skeleton "jhn smth") (abjad false) (reading Phonetic))
    ((tokens (allh bd mhmd)) (sorted "allh bd mhmd") (compact mhmdbdallh)
     (skeleton "bd lh mhmd") (abjad true) (reading Phonetic))
    ((tokens (abdullah muhammad)) (sorted "abdullah muhammad")
     (compact muhammadabdullah) (skeleton "bdlh mhmd") (abjad false)
     (reading Phonetic))
    |}]
;;

let gen_name =
  Quickcheck.Generator.union
    [ String.quickcheck_generator
    ; (let open Quickcheck.Generator.Let_syntax in
       let%bind cps =
         List.quickcheck_generator
           (Quickcheck.Generator.union
              [ Int.gen_incl 0x20 0x7E
              ; Int.gen_incl 0xC0 0x24F
              ; Int.gen_incl 0x370 0x3FF
              ; Int.gen_incl 0x400 0x4FF
              ; Int.gen_incl 0x600 0x6FF
              ; Int.gen_incl 0xFB50 0xFEFF
              ])
       in
       let buf = Buffer.create 16 in
       List.iter cps ~f:(Utf8.encode buf);
       return (Buffer.contents buf))
    ]
;;

let%expect_test "fold and key are idempotent" =
  Quickcheck.test gen_name ~sexp_of:String.sexp_of_t ~f:(fun s ->
    let once = Normalize.fold s in
    [%test_result: string] (Normalize.fold once) ~expect:once;
    let k = Normalize.key s in
    [%test_result: string] (Normalize.key k.sorted).sorted ~expect:k.sorted);
  [%expect {| |}]
;;

let%expect_test "word order does not change the key" =
  Quickcheck.test
    (List.quickcheck_generator (String.gen_with_length 4 Char.gen_lowercase))
    ~sexp_of:[%sexp_of: string list]
    ~f:(fun words ->
      let a = Normalize.key (String.concat ~sep:" " words) in
      let b = Normalize.key (String.concat ~sep:" " (List.rev words)) in
      [%test_result: string] b.sorted ~expect:a.sorted);
  [%expect {| |}]
;;
