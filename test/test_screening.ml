open! Core
open Sanctions_screening
open Fixture_lists

let show (r : Screening.Result.t) = print_endline (Decision.to_string_hum r.decision)

let%expect_test "onboarding: exact name with matching date of birth" =
  show (screen (person ~dob:"1961-03-12" ~nationalities:[ "RU" ] "Oleg Paramonov"));
  [%expect {|
    potential match
      ofac-sdn:9001 1000 High edit 'Oleg PARAMONOV' ~ 'Oleg Paramonov' dob=Exact nationality=RU
      uk-ofsi:15001 1000 High edit 'Oleg PARAMONOV' ~ 'Oleg Paramonov' dob=Exact nationality=RU
      eu-consolidated:7001 947 High tokens 'Oleg Ivanovich Paramonov' ~ 'Oleg Paramonov' dob=Exact nationality=RU
    |}]
;;

let%expect_test "date of birth mismatch lowers priority but keeps the hit" =
  show (screen (person ~dob:"1990-01-01" "Oleg Paramonov"));
  [%expect {|
    potential match
      ofac-sdn:9001 880 Medium edit 'Oleg PARAMONOV' ~ 'Oleg Paramonov' dob=Mismatch
      uk-ofsi:15001 880 Medium edit 'Oleg PARAMONOV' ~ 'Oleg Paramonov' dob=Mismatch
      eu-consolidated:7001 747 Low tokens 'Oleg Ivanovich Paramonov' ~ 'Oleg Paramonov' dob=Mismatch
    |}]
;;

let%expect_test "transliteration and script variants" =
  show (screen (person "Олег Парамонов"));
  show (screen (person "Muhammad Abdullah Kalbani"));
  show (screen (person "يوسف حداد الكلباني"));
  show (screen (person "Andreas Papathanasoglu"));
  show (screen (person "Dmitry Korzhikov"));
  [%expect {|
    potential match
      ofac-sdn:9001 1000 High edit 'Oleg PARAMONOV' ~ 'Олег Парамонов'
      uk-ofsi:15001 1000 High edit 'Oleg PARAMONOV' ~ 'Олег Парамонов'
      eu-consolidated:7001 867 Medium tokens 'Oleg Ivanovich Paramonov' ~ 'Олег Парамонов'
    clear
    potential match
      ofac-sdn:9101 1000 High edit 'يوسف حداد الكلباني' ~ 'يوسف حداد الكلباني'
      eu-consolidated:7005 898 Medium skeleton 'Yusuf Haddad Al-Kalbani' ~ 'يوسف حداد الكلباني'
    potential match
      eu-consolidated:7012 994 High tokens 'Andreas Papathanasoglou' ~ 'Andreas Papathanasoglu'
      ofac-sdn:9104 994 High tokens 'Andreas PAPATHANASOGLOU' ~ 'Andreas Papathanasoglu'
    potential match
      ofac-sdn:9002 986 High tokens 'Dmitriy KORZHIKOV' ~ 'Dmitry Korzhikov'
      uk-ofsi:15002 945 High tokens 'Dmitriy KORJIKOV' ~ 'Dmitry Korzhikov'
    |}]
;;

let%expect_test "clear names" =
  show (screen (person "Maria Garcia Lopez"));
  show (screen (person "Oleg Smirnov"));
  [%expect {|
    clear
    clear
    |}]
;;

let%expect_test "entities and vessels as payment counterparties" =
  let cp name =
    Subject.counterparty ~id:"payee-1" ~name () |> ok_exn
    |> screen
         ~policy:Policy.payments
         ~context:(Counterparty { role = Beneficiary; reference = "E2E-1" })
  in
  show (cp "Northwind Maritime Holdings Limited");
  show (cp "NORTHWIND MARITIME HOLDING");
  show (cp "OOO Severnyi Veter");
  show (cp "Silver Heron Shipping");
  show (cp "Acme Widgets Ltd");
  [%expect {|
    potential match
      ofac-sdn:9008 1000 High edit 'NORTHWIND MARITIME HOLDINGS LTD.' ~ 'Northwind Maritime Holdings Limited'
      uk-ofsi:15003 1000 High edit 'NORTHWIND MARITIME HOLDINGS LIMITED' ~ 'Northwind Maritime Holdings Limited'
    potential match
      ofac-sdn:9008 992 High tokens 'NORTHWIND MARITIME HOLDINGS LTD.' ~ 'NORTHWIND MARITIME HOLDING'
      uk-ofsi:15003 992 High tokens 'NORTHWIND MARITIME HOLDINGS LIMITED' ~ 'NORTHWIND MARITIME HOLDING'
    potential match
      eu-consolidated:7003 1000 High edit 'ООО «Северный ветер»' ~ 'OOO Severnyi Veter'
    clear
    clear
    |}]
;;

let%expect_test "type mismatch is a reason, not a removal" =
  show
    (screen
       (Subject.create ~id:"c" ~party:Entity ~names:[ "Svetlana Zhuravina" ] () |> ok_exn));
  [%expect {|
    potential match
      eu-consolidated:7002 850 Medium edit 'Svetlana Zhuravina' ~ 'Svetlana Zhuravina' type=entity/person
      uk-ofsi:15006 742 Low tokens 'Svetlana JURAVINA' ~ 'Svetlana Zhuravina' type=entity/person weak-alias
      ofac-sdn:9007 717 Low tokens 'Svetlana Petrovna ZHURAVINA' ~ 'Svetlana Zhuravina' type=entity/person
    |}]
;;

let%expect_test "auto-confirm needs the policy's rule and an exact date" =
  let policy = { Policy.default with confirm = Some { min_name_score = 950 } } in
  let r = screen ~policy (person ~dob:"1961-03-12" "Oleg Ivanovich Paramonov") in
  print_s [%sexp (Decision.customer_view r.decision : Decision.Customer_view.t)];
  print_endline (String.prefix (Decision.to_string_hum r.decision) 9);
  let r = screen ~policy (person "Oleg Ivanovich Paramonov") in
  print_endline (String.prefix (Decision.to_string_hum r.decision) 9);
  [%expect {|
    Under_review
    confirmed
    potential
    |}]
;;

let%expect_test "results are deterministic and carry their inputs" =
  let s = person ~dob:"1961-03-12" "Oleg Paramonov" in
  let a = screen s
  and b = screen s in
  print_s [%sexp (Screening.Result.equal a b : bool), (a.id : string)];
  print_s [%sexp (List.map a.lists ~f:(fun v -> List_id.to_string v.list, v.version) : (string * string) list)];
  let kyc = List.take (List.map (Decision.hits a.decision) ~f:Hit.to_kyc) 2 in
  print_s [%sexp (kyc : Kyc_core.Screening.Hit.t list)];
  [%expect {|
    (true bc4de5e440a8169565c242d20354aa99)
    ((internal-pep 2026-10-01) (eu-consolidated 2026-10-15) (ofac-sdn 2026-10-15)
     (ofac-sdn 2026-10-15) (uk-ofsi 2026-10-15))
    (((source Ofac_sdn) (list_version 2026-10-15) (entry_id 9001)
      (matched_name "Oleg PARAMONOV") (score 100)
      (algorithm sanctions-screening/edit))
     ((source Uk_sanctions_list) (list_version 2026-10-15) (entry_id 15001)
      (matched_name "Oleg PARAMONOV") (score 100)
      (algorithm sanctions-screening/edit)))
    |}]
;;

let%expect_test "the index finds what a full scan finds" =
  let lists = force lists in
  let index = Lists.index lists in
  let scratch = Match.Scratch.create () in
  let names =
    Array.to_list (Index.entries index)
    |> List.concat_map ~f:(fun (e : Entry.t) -> List.map e.names ~f:(fun n -> n.name))
  in
  let probes =
    names
    @ List.map names ~f:(fun n -> String.drop_suffix n 2)
    @ List.map names ~f:(fun n -> String.rev n)
    @ List.map (Adversarial.variants) ~f:snd
  in
  let missed = ref 0
  and total = ref 0 in
  List.iter probes ~f:(fun p ->
    List.iter (Normalize.keys p) ~f:(fun key ->
      let q = Match.Prepared.of_key key in
      let w = Policy.default.weights in
      let full = Index.scan index scratch ~weights:w ~threshold:850 q in
      let fast =
        Index.search index scratch ~weights:w ~prefilter:Policy.default.prefilter ~threshold:850 q
      in
      total := !total + List.length full;
      List.iter full ~f:(fun (id, sc) ->
        if not (List.mem fast (id, sc) ~equal:[%equal: int * int])
        then (
          incr missed;
          printf "MISS %s | %s %d\n" p (Index.name index id).name.name sc))));
  printf "names above 850: %d, missed by the prefilter: %d\n" !total !missed;
  [%expect {| names above 850: 670, missed by the prefilter: 0 |}]
;;
