open! Core
open Sanctions_screening

let%expect_test "CSV" =
  print_s
    [%sexp
      (Csv.parse
         "\xEF\xBB\xBFa,\"b, c\",\"say \"\"hi\"\"\"\r\n\r\n1,\"two\nlines\",\r\nlast\x1a"
       : string list list)];
  [%expect
    {|
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
  [%expect
    {|
    (root ("1 & 2") ((1 "One \195\169\195\169") (2 <raw>)))
    (1 2)
    true
    |}]
;;

let%expect_test "country names" =
  List.iter
    [ "Iran"; "IRAN"; "Russian Federation"; "Russia"; "ru"; "Côte d'Ivoire"; "Atlantis" ]
    ~f:(fun n -> print_s [%sexp (n : string), (Country.of_name n : string option)]);
  [%expect
    {|
    (Iran (IR))
    (IRAN (IR))
    ("Russian Federation" (RU))
    (Russia (RU))
    (ru (RU))
    ("C\195\180te d'Ivoire" (CI))
    (Atlantis ())
    |}]
;;

(* Reading a channel a chunk at a time must give the trees reading the whole string gives,
   wherever the chunk boundaries fall (inside names, attribute values with ">", entities,
   comments, CDATA). *)
let%expect_test "XML from a channel equals XML from a string, at any chunk size" =
  let rec show (t : Xml.t) =
    Sexp.List
      (Sexp.Atom t.name
       :: [%sexp (t.attrs : (string * string) list)]
       :: List.map t.children ~f:(function
         | Text s -> Sexp.Atom s
         | Element e -> show e))
  in
  let tricky =
    {|<?xml version="1.0"?><!-- a > comment --><root><a x="1 > 0" y='q"q'>t&amp;u &#x41;</a><![CDATA[<raw>]]><b/><c k="v" /></root>|}
  in
  let file = Filename_unix.temp_file "xml" ".xml" in
  let check ~depth contents =
    Out_channel.write_all file ~data:contents;
    let collect iter =
      let out = ref [] in
      ok_exn (iter ~f:(fun el -> out := show el :: !out));
      List.rev !out
    in
    let whole = collect (Xml.iter_elements contents ~depth) in
    List.for_all [ 1; 2; 3; 7; 64; 4096 ] ~f:(fun chunk_size ->
      let chunked =
        In_channel.with_file file ~f:(fun ic ->
          collect (Xml.iter_elements_channel ~chunk_size ic ~depth))
      in
      [%equal: Sexp.t list] whole chunked)
  in
  let fixture path = In_channel.read_all ("../fixtures/" ^ path) in
  let ofac = fixture "ofac-advanced/sdn_advanced.xml" in
  print_s
    [%message
      ""
        ~tricky:(List.map [ 0; 1; 2 ] ~f:(fun depth -> check ~depth tricky) : bool list)
        ~ofac:(check ~depth:2 ofac : bool)
        ~eu:(check ~depth:1 (fixture "eu/eu_consolidated.xml") : bool)
        ~ofac_snapshot:
          (Out_channel.write_all file ~data:ofac;
           [%equal: Entry.t list]
             (Ofac_xml.parse ofac |> ok_exn).entries
             (In_channel.with_file file ~f:Ofac_xml.of_channel |> ok_exn).entries
           : bool)
        ~truncated:
          (Out_channel.write_all file ~data:(String.prefix tricky 60);
           In_channel.with_file file ~f:(fun ic ->
             Xml.iter_elements_channel ~chunk_size:5 ic ~depth:1 ~f:ignore)
           |> Result.is_error
           : bool)];
  Core_unix.unlink file;
  [%expect
    {|
    ((tricky (true true true)) (ofac true) (eu true) (ofac_snapshot true)
     (truncated true))
    |}]
;;
