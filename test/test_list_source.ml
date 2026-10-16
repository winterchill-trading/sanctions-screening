open! Core
open Sanctions_screening

let read path = In_channel.read_all ("../fixtures/" ^ path)

let show_entry (e : Entry.t) =
  printf
    "%s %s %s\n"
    (Entry.key e)
    (Party_type.to_string e.party)
    (Option.value_map e.listed_on ~default:"" ~f:Date.to_string);
  List.iter e.names ~f:(fun n ->
    printf "  %s %s\n" (Sexp.to_string [%sexp (n.kind : Entry.Name.Kind.t)]) n.name);
  if not (List.is_empty e.dobs)
  then printf "  dob %s\n" (String.concat ~sep:", " (List.map e.dobs ~f:Dob.to_string));
  if not (List.is_empty e.nationalities)
  then printf "  nationality %s\n" (String.concat ~sep:", " e.nationalities);
  printf "  programmes %s\n" (String.concat ~sep:", " e.programmes)
;;

let show (s : Snapshot.t) =
  print_s [%sexp (s.version : List_version.t)];
  List.iter s.entries ~f:show_entry
;;

let%expect_test "OFAC SDN CSV" =
  Ofac_csv.parse
    ~version:"2026-10-15"
    ~sdn:(read "ofac/sdn.csv")
    ~alt:(read "ofac/alt.csv")
    ()
  |> ok_exn
  |> show;
  [%expect
    {|
    ((list Ofac_sdn) (version 2026-10-15) (published ()) (entries 18)
     (digest 7955f59098a1c078611678b23f814358))
    ofac-sdn:9001 person
      Primary Oleg Ivanovich PARAMONOV
      Alias Oleg PARAMONOV
      dob 1961-03-12
      nationality RU
      programmes RUSSIA-EO14024
    ofac-sdn:9002 person
      Primary Dmitri Valerevich KORZHIKOV
      Alias Dmitriy KORZHIKOV
      Alias Dmitri KORJIKOV
      dob 1975-07-04, 1976
      nationality BY, RU
      programmes CYBER2, RUSSIA-EO14024
    ofac-sdn:9003 person
      Primary Mahmoud Fathi AL-QUDSAYI
      Alias Mahmud QUDSAYI
      Weak_alias ABU FATHI
      dob 1967..1969
      nationality IQ
      programmes SDGT
    ofac-sdn:9004 person
      Primary Hassan Ali MOHAMMADZADEH-ROSTAMI
      Alias Hassan ROSTAMI
      dob 1959..1961
      nationality IR
      programmes IFSR, IRAN
    ofac-sdn:9005 person
      Primary Juan Carlos VELARDE OSTOS
      dob 1970-11-23
      nationality MX
      programmes SDNTK
    ofac-sdn:9006 person
      Primary Ahmad Shah NOORZAHI
      dob 1972
      nationality AF
      programmes SDGT
    ofac-sdn:9007 person
      Primary Svetlana Petrovna ZHURAVINA
      dob 1980-01-30
      nationality RU
      programmes RUSSIA-EO14024
    ofac-sdn:9008 entity
      Primary NORTHWIND MARITIME HOLDINGS LTD.
      Former NORTHWIND SHIPPING CO.
      programmes IRAN
    ofac-sdn:9009 entity
      Primary AL-NAJM GENERAL TRADING L.L.C.
      Alias NAJM TRADING
      Weak_alias AL NAJM
      programmes SDGT
    ofac-sdn:9010 entity
      Primary KESTREL AVIATION FZE
      programmes IRAN-EO13846
    ofac-sdn:9011 vessel
      Primary SILVER HERON
      programmes IRAN
    ofac-sdn:9012 vessel
      Primary OCEAN PEARL 7
      programmes DPRK3
    ofac-sdn:9013 entity
      Primary TRANSCASPIAN LOGISTICS JSC
      Alias TRANSKASPIYSKAYA LOGISTIKA AO
      programmes RUSSIA-EO14024
    ofac-sdn:9014 person
      Primary Nikolai Sergeevich BELOVODSKY
      dob 1966-08-15
      nationality RU
      programmes UKRAINE-EO13660
    ofac-sdn:9015 person
      Primary Viktor Andriyovych PIDHORETSKYI
      dob 1971-09
      nationality UA
      programmes UKRAINE-EO13660
    ofac-sdn:9016 entity
      Primary GOLDEN CRESCENT EXCHANGE CO.
      Alias AL-HILAL AL-DHAHABI EXCHANGE
      programmes SDGT
    ofac-sdn:9017 person
      Primary Ibrahim Abdullah AL-NAJDARI
      Alias Ibrahim NAJDARI
      dob 1964-01-01, 1965-01-01
      nationality KW, SA
      programmes SDGT
    ofac-sdn:9018 aircraft
      Primary KESTREL WING
      programmes IRAN
    |}]
;;

let%expect_test "OFAC remark dates" =
  List.iter
    [ "12 Mar 1961"
    ; "Sep 1971"
    ; "1972"
    ; "circa 1968"
    ; "1959 to 1961"
    ; "31 Feb 1960"
    ; "soon"
    ]
    ~f:(fun s -> print_s [%sexp (s : string), (Ofac_csv.parse_dob s : Dob.t option)]);
  [%expect
    {|
    ("12 Mar 1961" ((Day 1961-03-12)))
    ("Sep 1971" ((Month (year 1971) (month 9))))
    (1972 ((Year 1972)))
    ("circa 1968" ((Years (from 1967) (until 1969))))
    ("1959 to 1961" ((Years (from 1959) (until 1961))))
    ("31 Feb 1960" ())
    (soon ())
    |}]
;;

let%expect_test "OFAC advanced XML" =
  Ofac_xml.parse (read "ofac-advanced/sdn_advanced.xml") |> ok_exn |> show;
  [%expect
    {|
    ((list Ofac_sdn) (version 2026-10-15) (published ()) (entries 5)
     (digest 92782392cc383217993c32066539cbef))
    ofac-sdn:9101 person 2019-03-14
      Primary Yusuf Haddad KALBANI
      Alias يوسف حداد الكلباني
      Weak_alias ABU YUSUF
      dob 1970-05-02
      nationality SY
      programmes SYRIA
    ofac-sdn:9102 entity 2022-04-06
      Primary EASTMOOR BANK PJSC
      Former EASTMOOR COMMERCIAL BANK
      programmes RUSSIA-EO14024
    ofac-sdn:9103 vessel
      Primary NORTHERN DAWN
      programmes IRAN, SDGT
    ofac-sdn:9104 person
      Primary Andreas PAPATHANASOGLOU
      Alias Ανδρέας Παπαθανασόγλου
      dob 1958
      nationality CY
      programmes CYPRUS-TEST
    ofac-sdn:9105 person
      Primary Rashid Kareem TALEBZADEH
      dob 1962..1964
      nationality RU
      programmes RUSSIA-EO14024
    |}]
;;

let%expect_test "EU consolidated XML" =
  Eu_xml.parse (read "eu/eu_consolidated.xml") |> ok_exn |> show;
  [%expect
    {|
    ((list Eu_consolidated) (version 2026-10-15) (published ()) (entries 12)
     (digest 622e96ba5f4856b8a02b1e62919a1df3))
    eu-consolidated:7001 person 2022-02-23
      Primary Oleg Ivanovich Paramonov
      Alias Олег Иванович Парамонов
      dob 1961-03-12
      nationality RU
      programmes RUS
    eu-consolidated:7002 person 2022-03-15
      Primary Svetlana Petrovna Zhuravina
      Alias Svetlana Zhuravina
      dob 1980-01-30
      nationality RU
      programmes RUS
    eu-consolidated:7003 entity 2022-06-03
      Primary Obshchestvo s ogranichennoi otvetstvennostiu Severnyi Veter
      Alias ООО «Северный ветер»
      Alias Northern Wind LLC
      programmes RUS
    eu-consolidated:7004 entity 2023-02-25
      Primary Transcaspian Logistics JSC
      Alias АО «Транскаспийская логистика»
      programmes RUS
    eu-consolidated:7005 person 2012-05-14
      Primary Yusuf Haddad Al-Kalbani
      Weak_alias Abu Yusuf
      dob 1969..1971
      nationality SY
      programmes SYR
    eu-consolidated:7006 person 2013-10-10
      Primary Hassan Ali Mohammadzadeh Rostami
      dob 1960
      nationality IR
      programmes IRN
    eu-consolidated:7007 entity 2013-10-10
      Primary Kestrel Aviation FZE
      programmes IRN
    eu-consolidated:7008 person 2014-03-17
      Primary Viktor Andriyovych Pidhoretskyi
      Alias Віктор Андрійович Підгорецький
      Alias Viktor Andreevich Podgoretsky
      dob 1971-09-21
      nationality UA
      programmes UKR
    eu-consolidated:7009 person 2014-03-17
      Primary Nikolai Sergeevich Belovodsky
      Alias Mykola Serhiyovych Bilovodskyi
      dob 1966-08-15
      nationality RU
      programmes UKR
    eu-consolidated:7010 person 2003-07-07
      Primary Mahmoud Fathi Al-Qudsayi
      Alias محمود فتحي القدسي
      dob 1967..1969
      nationality IQ
      programmes TAQA
    eu-consolidated:7011 person 2021-06-21
      Primary Aliaksandr Uladzimiravich Shchapatsilnikau
      Alias Александр Владимирович Щепетильников
      dob 1974-11-02
      nationality BY
      programmes BLR
    eu-consolidated:7012 person 2020-01-01
      Primary Andreas Papathanasoglou
      Alias Ανδρέας Παπαθανασόγλου
      dob 1958
      nationality CY
      programmes GRC-TEST
    |}]
;;

let%expect_test "UK OFSI CSV" =
  Uk_csv.parse (read "uk/ConList.csv") |> ok_exn |> show;
  [%expect
    {|
    ((list Uk_ofsi) (version 2026-10-15) (published ()) (entries 12)
     (digest 361141ba8d3ea0f4f6f164718fcfa43b))
    uk-ofsi:15001 person 2022-03-15
      Primary Oleg Ivanovich PARAMONOV
      Alias Олег Иванович Парамонов
      Alias Oleg PARAMONOV
      dob 1961-03-12
      nationality RU
      programmes Russia
    uk-ofsi:15002 person 2022-03-15
      Primary Dmitri Valerevich KORZHIKOV
      Alias Dmitriy KORJIKOV
      dob 1975-07-04
      nationality BY, RU
      programmes Russia
    uk-ofsi:15003 entity 2022-03-15
      Primary NORTHWIND MARITIME HOLDINGS LIMITED
      Former NORTHWIND SHIPPING COMPANY
      programmes Iran
    uk-ofsi:15004 vessel 2024-02-01
      Primary SILVER HERON
      programmes Iran
    uk-ofsi:15005 person 2022-03-15
      Primary Juan Carlos VELARDE OSTOS
      dob 1970-11-23
      nationality MX
      programmes Global Human Rights
    uk-ofsi:15006 person 2022-03-15
      Primary Svetlana Petrovna ZHURAVINA
      Weak_alias Svetlana JURAVINA
      dob 1980-01-30
      nationality RU
      programmes Russia
    uk-ofsi:15007 person 2022-03-15
      Primary Ibrahim Abdullah AL-NAJDARI
      dob 1964
      nationality KW
      programmes ISIL (Da'esh) and Al-Qaida
    uk-ofsi:15008 entity 2022-03-15
      Primary GOLDEN CRESCENT EXCHANGE CO
      programmes Counter-Terrorism (International)
    uk-ofsi:15009 entity 2022-03-15
      Primary EASTMOOR BANK PJSC
      programmes Russia
    uk-ofsi:15010 entity 2022-03-15
      Primary BLUE LANTERN EXPORT GMBH
      programmes Russia
    uk-ofsi:15011 person 2022-03-15
      Primary Ahmad Shah NOORZAHI
      dob 1972
      nationality AF
      programmes Afghanistan
    uk-ofsi:15012 person 2022-03-15
      Primary Aleksandr Vladimirovich SHCHEPETILNIKOV
      dob 1974-11-02
      nationality BY
      programmes Belarus
    |}]
;;

let%expect_test "UK Sanctions List XML" =
  let text = read "uk-sanctions-list/UK-Sanctions-List.xml" in
  Uk_xml.parse text |> ok_exn |> show;
  [%expect
    {|
    ((list Uk_sanctions_list) (version 2026-10-15) (published ()) (entries 4)
     (digest c464ba9ccbb352093941227e60068193))
    uk-sanctions-list:RUS9001 person 2023-03-17
      Primary Radomir Vasilyevich ZHELTOVSKY
      Alias Radomir ZHELTOVSKIY
      Alias Radomir ZHELTOVSKI
      Alias Радомир Васильевич Желтовский
      Weak_alias Rado Zhelt
      dob 1968-08-23
      nationality BY, RU
      programmes The Russia (Sanctions) (EU Exit) Regulations 2019
    uk-sanctions-list:RUS9003 entity 2025-09-09
      Primary AMBERFIELD TRADING FZE
      Alias Amberfield Trading
      programmes The Russia (Sanctions) (EU Exit) Regulations 2019
    uk-sanctions-list:RUS9004 vessel 2025-05-20
      Primary CORAL MERIDIAN
      Alias Meridian Star 4
      programmes The Russia (Sanctions) (EU Exit) Regulations 2019
    uk-sanctions-list:SYR9002 person 2024-12-01
      Primary Farid Nasser AL-QUDAIMI
      Alias Fareed QUDAIMI
      dob 1960-04, 1959
      nationality SY
      programmes The Syria (Sanctions) (EU Exit) Regulations 2019
    |}];
  print_s
    [%sexp
      (List_source.detect ~filename:"UK-Sanctions-List.xml" ~contents:text
       : List_source.Format.t option)];
  [%expect {| (Uk_sanctions_list_xml) |}];
  (* Date forms seen in the published file, including its one malformed date. *)
  let dobs =
    List.map
      [ "23/08/1968"; "dd/mm/1959"; "dd/04/1960"; "1957"; "11/11/19yy"; "31/02/1970" ]
      ~f:(fun d ->
        let doc =
          sprintf
            {|<Designations><DateGenerated>15/10/2026</DateGenerated><Designation><UniqueID>X1</UniqueID><Names><Name><Name6>A B</Name6><NameType>Primary name</NameType></Name></Names><IndividualEntityShip>Individual</IndividualEntityShip><IndividualDetails><Individual><DOBs><DOB>%s</DOB></DOBs></Individual></IndividualDetails></Designation></Designations>|}
            d
        in
        let s = Uk_xml.parse doc |> ok_exn in
        d, List.concat_map s.entries ~f:(fun e -> List.map e.dobs ~f:Dob.to_string))
  in
  print_s [%sexp (dobs : (string * string list) list)];
  [%expect
    {|
    ((23/08/1968 (1968-08-23)) (dd/mm/1959 (1959)) (dd/04/1960 (1960-04))
     (1957 (1957)) (11/11/19yy ()) (31/02/1970 ()))
    |}]
;;

let%expect_test "fixture round trip" =
  let s = Fixture.parse (read "custom/internal-pep.sexp") |> ok_exn in
  show s;
  let again = Fixture.parse (Fixture.to_string s) |> ok_exn in
  print_s [%sexp (List.equal Entry.equal s.entries again.entries : bool)];
  [%expect
    {|
    ((list (Other internal-pep)) (version 2026-10-01) (published (2026-10-01))
     (entries 4) (digest 0e38bb2f80b33a33aa3d699365206c44))
    internal-pep:PEP-001 person
      Primary Marta Kowalczyk-Brenner
      Alias Marta Brenner
      dob 1969-04-17
      nationality PL
      programmes PEP-domestic
    internal-pep:PEP-002 person
      Primary Tomasz Wieczorek-Lind
      dob 1955
      nationality PL
      programmes PEP-RCA
    internal-pep:PEP-003 person
      Primary Georgios Antonakopoulos
      Alias Γεώργιος Αντωνακόπουλος
      dob 1962-09
      nationality GR
      programmes PEP-foreign
    internal-pep:WATCH-001 entity
      Primary Blue Lantern Export GmbH
      programmes internal-watch
    true
    |}]
;;

let%expect_test "load a directory" =
  List_source.load_dir "../fixtures"
  |> ok_exn
  |> List.iter ~f:(fun (s : Snapshot.t) ->
    printf
      "%s %s %d %s\n"
      (List_id.to_string s.version.list)
      s.version.version
      s.version.entries
      (String.prefix s.version.digest 8));
  [%expect
    {|
    internal-pep 2026-10-01 4 0e38bb2f
    eu-consolidated 2026-10-15 12 622e96ba
    ofac-sdn 2026-10-15 18 7955f590
    ofac-sdn 2026-10-15 5 92782392
    uk-ofsi 2026-10-15 12 361141ba
    uk-sanctions-list 2026-10-15 4 c464ba9c
    |}]
;;

let%expect_test "malformed input fails with a reason" =
  print_s
    [%sexp (Eu_xml.parse "<export><sanctionEntity></export>" : Snapshot.t Or_error.t)];
  print_s [%sexp (Uk_csv.parse "" : Snapshot.t Or_error.t)];
  [%expect
    {|
    (Error ("malformed XML" "</export> closes <sanctionEntity>"))
    (Error "empty UK list file")
    |}]
;;

(* The consumer case: no fixtures directory next to the binary. The embedded lists are the
   committed files, parsed the same way, and [load_files] reads lists held in memory. *)
let%expect_test "bundled fixtures equal the fixtures directory" =
  let from_dir = List_source.load_dir "../fixtures" |> ok_exn in
  let bundled = List_source.load_fixtures () |> ok_exn in
  let versions l = List.map l ~f:(fun (s : Snapshot.t) -> s.version) in
  print_s
    [%message
      ""
        ~equal:
          ([%equal: Entry.t list list]
             (List.map from_dir ~f:(fun s -> s.entries))
             (List.map bundled ~f:(fun s -> s.entries))
           : bool)
        ~same_versions:
          ([%equal: List_version.t list] (versions from_dir) (versions bundled) : bool)
        ~files:(List.map (List_source.fixture_files ()) ~f:fst : string list)
        ~lists:(Lists.create bundled |> Result.is_ok : bool)];
  let without_version =
    List_source.fixture_files ()
    |> List.filter ~f:(fun (p, _) -> String.is_prefix p ~prefix:"ofac/")
    |> List.filter ~f:(fun (p, _) -> not (String.equal p "ofac/VERSION"))
    |> List_source.load_files
    |> ok_exn
  in
  print_s [%sexp (versions without_version : List_version.t list)];
  print_s
    [%sexp
      (List_source.load_files [ "a.sexp", "()"; "a.sexp", "()" ] |> Result.is_error
       : bool)];
  [%expect
    {|
    ((equal true) (same_versions true)
     (files
      (custom/internal-pep.sexp eu/eu_consolidated.xml
       ofac-advanced/sdn_advanced.xml ofac/VERSION ofac/alt.csv ofac/sdn.csv
       uk-sanctions-list/UK-Sanctions-List.xml uk/ConList.csv))
     (lists true))
    (((list Ofac_sdn) (version unknown) (published ()) (entries 18)
      (digest 7955f59098a1c078611678b23f814358)))
    true
    |}]
;;
