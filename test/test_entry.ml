open! Core
open Sanctions_screening

let%expect_test "date of birth parsing" =
  List.iter
    [ "1960-03-12"
    ; "1960-03"
    ; "1960"
    ; "1959..1961"
    ; "circa 1960"
    ; "1960-13"
    ; "60"
    ; "1962..1961"
    ]
    ~f:(fun s ->
      print_s
        [%sexp
          (s : string)
          , (Dob.of_string s |> Or_error.map ~f:Dob.to_string : string Or_error.t)]);
  [%expect
    {|
    (1960-03-12 (Ok 1960-03-12))
    (1960-03 (Ok 1960-03))
    (1960 (Ok 1960))
    (1959..1961 (Ok 1959..1961))
    ("circa 1960" (Ok 1959..1961))
    (1960-13 (Error ("bad month" 1960-13)))
    (60 (Error ("bad year" 60)))
    (1962..1961 (Error ("empty year range" 1962..1961)))
    |}]
;;

let%expect_test "date of birth agreement" =
  let dob s = Dob.of_string s |> ok_exn in
  let show subject entry =
    print_s
      [%sexp
        (subject : string)
        , (entry : string list)
        , (Dob.agreement ~tolerance_years:2 (dob subject) (List.map entry ~f:dob)
           : Dob.Agreement.t option)]
  in
  show "1960-03-12" [ "1960-03-12" ];
  show "1960-03-12" [ "1960-03" ];
  show "1960-03-12" [ "1960-07-01" ];
  show "1960-03-12" [ "1958..1959" ];
  show "1960-03-12" [ "1950"; "1960-03-12" ];
  show "1960" [ "1970" ];
  show "1960" [];
  [%expect
    {|
    (1960-03-12 (1960-03-12) (Exact))
    (1960-03-12 (1960-03) (Same_month))
    (1960-03-12 (1960-07-01) (Same_year))
    (1960-03-12 (1958..1959) ((Within_years 1)))
    (1960-03-12 (1950 1960-03-12) (Exact))
    (1960 (1970) (Mismatch))
    (1960 () ())
    |}]
;;

let entry ?(version = "v1") ?(names = [ "Oleg Paramonov" ]) id =
  Entry.create
    ~list:(Other "test")
    ~id
    ~version
    ~party:Person
    ~names:
      (List.mapi names ~f:(fun i name ->
         { Entry.Name.name; kind = (if i = 0 then Primary else Alias) }))
    ~nationalities:[ " ru"; "RU"; "by" ]
    ~programmes:[ "B"; "A" ]
    ()
  |> ok_exn
;;

let%expect_test "entry normalises its lists" =
  print_s
    [%sexp
      (Entry.create
         ~list:(Other "test")
         ~id:"7"
         ~version:"v1"
         ~party:Person
         ~names:
           [ { name = "  "; kind = Alias }
           ; { name = "Alias One"; kind = Alias }
           ; { name = "Main Name"; kind = Primary }
           ; { name = "Alias One"; kind = Weak_alias }
           ]
         ~nationalities:[ "ru"; " RU " ]
         ()
       : Entry.t Or_error.t)];
  print_s
    [%sexp
      (Entry.create ~list:(Other "test") ~id:"8" ~version:"v1" ~party:Person ~names:[] ()
       : Entry.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((list (Other test)) (id 7) (version v1) (party Person)
      (names
       (((name "Main Name") (kind Primary)) ((name "Alias One") (kind Alias))))
      (dobs ()) (nationalities (RU)) (programmes ()) (listed_on ())))
    (Error ("entry without a name" (list (Other test)) 8))
    |}]
;;

let%expect_test "snapshot diff" =
  let snap version entries =
    Snapshot.create ~list:(Other "test") ~version entries |> ok_exn
  in
  let before = snap "v1" [ entry "1"; entry "2"; entry "3" ] in
  let after =
    snap
      "v2"
      [ entry ~version:"v2" "1"
      ; entry ~version:"v2" ~names:[ "Oleg Paramonov"; "O. Paramonov" ] "2"
      ; entry ~version:"v2" "4"
      ]
  in
  let d = Snapshot.diff ~before ~after in
  let ids es = List.map es ~f:(fun (e : Entry.t) -> e.id) in
  print_s
    [%sexp
      { added = (ids d.added : string list)
      ; removed = (ids d.removed : string list)
      ; changed = (ids (List.map d.changed ~f:snd) : string list)
      }];
  print_s
    [%sexp
      (Snapshot.create ~list:(Other "test") ~version:"v" [ entry "1"; entry "1" ]
       : Snapshot.t Or_error.t)];
  [%expect
    {|
    ((added (4)) (removed (3)) (changed (2)))
    (Error ("duplicate entry id" (id 1)))
    |}]
;;

let%expect_test "stable digests" =
  print_endline [%bin_digest: Entry.Stable.V1.t];
  print_endline [%bin_digest: List_version.Stable.V1.t];
  print_endline [%bin_digest: Dob.Stable.V1.t];
  print_endline [%bin_digest: Party_type.Stable.V1.t];
  print_endline [%bin_digest: List_id.Stable.V1.t];
  [%expect
    {|
    432092396d044828e17e04b5a6f03e03
    88354b7c927a9fb5dd7977395f9e5ca8
    61e3178b812c55da134c23f84bf692c4
    448f6a8b27e74b837a5ef73914bc2687
    efccf4149a1e6281aba28b73fbdcefb3
    |}]
;;
