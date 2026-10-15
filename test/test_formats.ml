open! Core
open Sanctions_screening

let%expect_test "CSV" =
  print_s
    [%sexp
      (Csv.parse
         "\xEF\xBB\xBFa,\"b, c\",\"say \"\"hi\"\"\"\r\n\r\n1,\"two\nlines\",\r\nlast\x1a"
       : string list list)];
  [%expect {|
    ((a "b, c" "say \"hi\"") (1  "two\
                                \nlines" "") (last))
    |}]
;;

let%expect_test "XML" =
  let doc =
    {|<?xml version="1.0"?>
<!-- comment -->
<ns:root xmlns:ns="urn:x" a="1 &amp; 2">
  <item id="1">One &#233;&#x00E9;</item>
  <item id="2"><![CDATA[<raw>]]><sub/></item>
</ns:root>|}
  in
  let root = Xml.parse doc |> ok_exn in
  print_s
    [%sexp
      (root.name : string)
      , (Xml.attr root "a" : string option)
      , (List.map (Xml.children root "item") ~f:(fun i ->
           Xml.attr_or_empty i "id", Xml.text i)
         : (string * string) list)];
  let ids = ref [] in
  Xml.iter_elements doc ~depth:1 ~f:(fun el -> ids := Xml.attr_or_empty el "id" :: !ids)
  |> ok_exn;
  print_s [%sexp (List.rev !ids : string list)];
  print_s [%sexp (Result.is_error (Xml.parse "<a><b></a>") : bool)];
  [%expect {|
    (root ("1 & 2") ((1 "One \195\169\195\169") (2 <raw>)))
    (1 2)
    true
    |}]
;;

let%expect_test "country names" =
  List.iter
    [ "Iran"; "IRAN"; "Russian Federation"; "Russia"; "ru"; "Côte d'Ivoire"; "Atlantis" ]
    ~f:(fun n -> print_s [%sexp (n : string), (Country.of_name n : string option)]);
  [%expect {|
    (Iran (IR))
    (IRAN (IR))
    ("Russian Federation" (RU))
    (Russia (RU))
    (ru (RU))
    ("C\195\180te d'Ivoire" (CI))
    (Atlantis ())
    |}]
;;
