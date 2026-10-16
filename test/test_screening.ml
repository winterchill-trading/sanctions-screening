open! Core
open Sanctions_screening
open Fixture_lists

let show (r : Screening.Result.t) = print_endline (Decision.to_string_hum r.decision)

let%expect_test "onboarding: exact name with matching date of birth" =
  show (screen (person ~dob:"1961-03-12" ~nationalities:[ "RU" ] "Oleg Paramonov"));
  [%expect
    {|
    potential match
      eu-consolidated:7001 1000 High tokens 'Oleg Ivanovich Paramonov' ~ 'Oleg Paramonov' dob=Exact nationality=RU
      ofac-sdn:9001 1000 High tokens 'Oleg Ivanovich PARAMONOV' ~ 'Oleg Paramonov' dob=Exact nationality=RU
      uk-ofsi:15001 1000 High tokens 'Oleg Ivanovich PARAMONOV' ~ 'Oleg Paramonov' dob=Exact nationality=RU
    |}]
;;

let%expect_test "date of birth mismatch lowers priority but keeps the hit" =
  show (screen (person ~dob:"1990-01-01" "Oleg Paramonov"));
  [%expect
    {|
    potential match
      eu-consolidated:7001 880 Medium tokens 'Oleg Ivanovich Paramonov' ~ 'Oleg Paramonov' dob=Mismatch
      ofac-sdn:9001 880 Medium tokens 'Oleg Ivanovich PARAMONOV' ~ 'Oleg Paramonov' dob=Mismatch
      uk-ofsi:15001 880 Medium tokens 'Oleg Ivanovich PARAMONOV' ~ 'Oleg Paramonov' dob=Mismatch
    |}]
;;

let%expect_test "transliteration and script variants" =
  show (screen (person "Олег Парамонов"));
  show (screen (person "Muhammad Abdullah Kalbani"));
  show (screen (person "يوسف حداد الكلباني"));
  show (screen (person "Andreas Papathanasoglu"));
  show (screen (person "Dmitry Korzhikov"));
  [%expect
    {|
    potential match
      eu-consolidated:7001 1000 High tokens 'Oleg Ivanovich Paramonov' ~ 'Олег Парамонов'
      ofac-sdn:9001 1000 High tokens 'Oleg Ivanovich PARAMONOV' ~ 'Олег Парамонов'
      uk-ofsi:15001 1000 High tokens 'Oleg Ivanovich PARAMONOV' ~ 'Олег Парамонов'
    clear
    potential match
      ofac-sdn:9101 1000 High edit 'يوسف حداد الكلباني' ~ 'يوسف حداد الكلباني'
      eu-consolidated:7005 898 Medium skeleton 'Yusuf Haddad Al-Kalbani' ~ 'يوسف حداد الكلباني'
    potential match
      eu-consolidated:7012 994 High tokens 'Andreas Papathanasoglou' ~ 'Andreas Papathanasoglu'
      ofac-sdn:9104 994 High tokens 'Andreas PAPATHANASOGLOU' ~ 'Andreas Papathanasoglu'
    potential match
      ofac-sdn:9002 986 High tokens 'Dmitriy KORZHIKOV' ~ 'Dmitry Korzhikov'
      uk-ofsi:15002 967 High tokens 'Dmitri Valerevich KORZHIKOV' ~ 'Dmitry Korzhikov'
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
    Subject.counterparty ~id:"payee-1" ~name ()
    |> ok_exn
    |> screen
         ~policy:Policy.payments
         ~context:(Counterparty { role = Beneficiary; reference = "E2E-1" })
  in
  show (cp "Northwind Maritime Holdings Limited");
  show (cp "NORTHWIND MARITIME HOLDING");
  show (cp "OOO Severnyi Veter");
  show (cp "Silver Heron Shipping");
  show (cp "Acme Widgets Ltd");
  [%expect
    {|
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
  [%expect
    {|
    potential match
      eu-consolidated:7002 850 Medium tokens 'Svetlana Petrovna Zhuravina' ~ 'Svetlana Zhuravina' type=entity/person
      ofac-sdn:9007 850 Medium tokens 'Svetlana Petrovna ZHURAVINA' ~ 'Svetlana Zhuravina' type=entity/person
      uk-ofsi:15006 850 Medium tokens 'Svetlana Petrovna ZHURAVINA' ~ 'Svetlana Zhuravina' type=entity/person
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
  print_s
    [%sexp
      (List.map a.lists ~f:(fun v -> List_id.to_string v.list, v.version)
       : (string * string) list)];
  let kyc = List.take (List.map (Decision.hits a.decision) ~f:Hit.to_kyc) 2 in
  print_s [%sexp (kyc : Kyc_core.Screening.Hit.t list)];
  [%expect
    {|
    (true 8415e0dbd04f8a4c98d4a717fdc6b14b)
    ((internal-pep 2026-10-01) (eu-consolidated 2026-10-15) (ofac-sdn 2026-10-15)
     (ofac-sdn 2026-10-15) (uk-ofsi 2026-10-15) (uk-sanctions-list 2026-10-15))
    (((source Eu_fsf) (list_version 2026-10-15) (entry_id eu-consolidated:7001)
      (matched_name "Oleg Ivanovich Paramonov") (score 100)
      (algorithm sanctions-screening/tokens))
     ((source Ofac_sdn) (list_version 2026-10-15) (entry_id ofac-sdn:9001)
      (matched_name "Oleg Ivanovich PARAMONOV") (score 100)
      (algorithm sanctions-screening/tokens)))
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
    @ List.map Adversarial.variants ~f:snd
  in
  let missed = ref 0
  and total = ref 0 in
  List.iter probes ~f:(fun p ->
    List.iter (Normalize.keys p) ~f:(fun key ->
      let q = Match.Prepared.of_key key in
      let w = Policy.default.weights in
      let full = Index.scan index scratch ~weights:w ~threshold:850 q in
      let fast =
        Index.search
          index
          scratch
          ~weights:w
          ~prefilter:Policy.default.prefilter
          ~threshold:850
          q
      in
      total := !total + List.length full;
      List.iter full ~f:(fun (id, sc) ->
        if not (List.mem fast (id, sc) ~equal:[%equal: int * int])
        then (
          incr missed;
          printf "MISS %s | %s %d\n" p (Index.name index id).name.name sc))));
  printf "names above 850: %d, missed by the prefilter: %d\n" !total !missed;
  [%expect {| names above 850: 729, missed by the prefilter: 0 |}]
;;

(* Two lists may use the same entry id. kyc-core identifies a hit within one screening by
   its entry id, so the kyc record carries the list-qualified key; the raw id stays on the
   hit and can be read back from the key. *)
let%expect_test "kyc hits are keyed by list and entry id" =
  let list name =
    sprintf
      {|((list (Other %s)) (version 2026-10-01)
         (entries (((id 101) (party Person)
                    (names (((name "Oleg Paramonov") (kind Primary))))))))|}
      name
    |> Fixture.parse
    |> ok_exn
  in
  let lists = Lists.create [ list "watch-a"; list "watch-b" ] |> ok_exn in
  let result =
    Screening.screen
      ~policy:Policy.default
      ~lists
      ~context:Onboarding
      ~at
      (person "Oleg Paramonov")
  in
  let hits = Decision.hits result.decision in
  let kyc = List.map hits ~f:Hit.to_kyc in
  print_s
    [%message
      ""
        ~raw_ids:(List.map hits ~f:(fun h -> h.entry_id) : string list)
        ~kyc_ids:(List.map kyc ~f:(fun h -> h.entry_id) : string list)
        ~read_back:
          (List.map kyc ~f:(fun h -> Hit.split_entry_key h.entry_id)
           : (List_id.t * string) option list)
        ~kyc_screening_accepts:
          (Kyc_core.Screening.create
             ~id:(Kyc_core.Screening_id.of_string "screening-1")
             ~reason:Onboarding
             ~screened_at:at
             ~hits:kyc
           |> Result.is_ok
           : bool)];
  [%expect
    {|
    ((raw_ids (101 101)) (kyc_ids (watch-a:101 watch-b:101))
     (read_back ((((Other watch-a) 101)) (((Other watch-b) 101))))
     (kyc_screening_accepts true))
    |}]
;;
