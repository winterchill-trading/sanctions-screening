open! Core
open Sanctions_screening

let show s = print_endline (Unicode_fold.fold s)

let%expect_test "decomposes, drops marks, lowercases" =
  show "Ĳsselmeer Ångström";
  show "ÉCOLE Ⅻ ﬁnance";
  show "Øresund Łódź";
  show "ПЁТР Йошкар";
  show "Ἀθῆναι";
  show "ﺃﺣﻤﺪ";
  show "ＡＢＣ１２３";
  [%expect
    {|
    ijsselmeer angstrom
    ecole xii finance
    øresund łodz
    петр иошкар
    αθηναι
    احمد
    abc123
    |}]
;;

let%expect_test "malformed UTF-8 becomes U+FFFD, one byte at a time" =
  show "a\xC3b\xFF";
  [%expect {| a�b� |}];
  let packed = Utf8.decode "\xE2\x82\xAC" ~pos:0 in
  printf "U+%04X in %d bytes\n" (Utf8.code_point packed) (Utf8.length packed);
  [%expect {| U+20AC in 3 bytes |}]
;;

let%expect_test "fold is idempotent" =
  Quickcheck.test
    ~sexp_of:String.sexp_of_t
    (Quickcheck.Generator.union
       [ String.quickcheck_generator
       ; (let%bind.Quickcheck.Generator cps =
            List.quickcheck_generator (Int.gen_incl 0 0xFFFF)
          in
          let buf = Buffer.create 16 in
          List.iter cps ~f:(Utf8.encode buf);
          Quickcheck.Generator.return (Buffer.contents buf))
       ])
    ~f:(fun s ->
      let once = Unicode_fold.fold s in
      [%test_result: string] (Unicode_fold.fold once) ~expect:once);
  [%expect {| |}]
;;

let%expect_test "table version" =
  print_endline Unicode_fold.unicode_version;
  [%expect {| 16.0.0 |}]
;;
