open! Core
open Sanctions_screening
open Fixture_lists

let%expect_test "rescreen after a list update reports newly hit and cleared subjects" =
  let before = force lists in
  let ofac =
    List.find_exn (Lists.snapshots before) ~f:(fun s ->
      List_id.equal s.version.list Ofac_sdn && s.version.entries = 18)
  in
  let added =
    Entry.create
      ~list:Ofac_sdn
      ~id:"9020"
      ~version:"2026-10-16"
      ~party:Person
      ~names:[ { name = "Pavel Gennadievich OSTROVERKHOV"; kind = Primary } ]
      ~dobs:[ Year 1979 ]
      ()
    |> ok_exn
  in
  let entries =
    added :: List.filter ofac.entries ~f:(fun e -> not (String.equal e.id "9018"))
    |> List.map ~f:(fun (e : Entry.t) ->
      if String.equal e.id "9006"
      then { e with names = e.names @ [ { name = "Ahmad Shah NURZAI"; kind = Alias } ] }
      else e)
  in
  let new_ofac = Snapshot.create ~list:Ofac_sdn ~version:"2026-10-16" entries |> ok_exn in
  let after =
    Lists.create
      (List.map (Lists.snapshots before) ~f:(fun s ->
         if phys_equal s ofac then new_ofac else s))
    |> ok_exn
  in
  let subject id name =
    let s = Subject.create ~id ~names:[ name ] () |> ok_exn in
    s, (screen s).decision
  in
  let population =
    [ subject "c-1" "Pavel Ostroverkhov"
    ; subject "c-2" "Kestrel Wing"
    ; subject "c-3" "Ahmad Shah Nurzai"
    ; subject "c-4" "Oleg Paramonov"
    ; subject "c-5" "Maria Garcia Lopez"
    ; subject "c-6" "Petra Novak"
    ]
  in
  let r =
    Screening.rescreen ~policy:Policy.default ~before ~after ~at population |> ok_exn
  in
  List.iter r.diff ~f:(fun (list, d) ->
    if not (Snapshot.Diff.is_empty d)
    then
      printf
        "%s: +%d -%d ~%d\n"
        (List_id.to_string list)
        (List.length d.added)
        (List.length d.removed)
        (List.length d.changed));
  let ids (cs : Screening.Rescreen.change list) =
    List.map cs ~f:(fun c ->
      c.subject.id, List.map (Decision.hits c.result.decision) ~f:Hit.entry_key)
  in
  print_s
    [%sexp
      { newly_hit = (ids r.newly_hit : (string * string list) list)
      ; cleared = (ids r.cleared : (string * string list) list)
      ; rescreened = (r.rescreened : int)
      ; unchanged = (r.unchanged : int)
      }];
  [%expect
    {|
    ofac-sdn: +1 -1 ~1
    ((newly_hit ((c-1 (ofac-sdn:9020)))) (cleared ((c-2 ()))) (rescreened 3)
     (unchanged 3))
    |}]
;;
