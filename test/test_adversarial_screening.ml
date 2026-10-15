open! Core
open Sanctions_screening

(* The listed name is on a small list of its own, so every disguise must find exactly
   that entry. Also checked against the payments policy, which has the higher
   threshold. *)
let lists =
  lazy
    (let entry =
       Entry.create
         ~list:(Other "adversarial")
         ~id:"A-1"
         ~version:"v1"
         ~party:Person
         ~names:[ { name = Adversarial.listed; kind = Primary } ]
         ()
       |> ok_exn
     in
     Lists.create [ Snapshot.create ~list:(Other "adversarial") ~version:"v1" [ entry ] |> ok_exn ]
     |> ok_exn)
;;

let%expect_test "every disguised spelling still hits" =
  let at = Time_ns.epoch in
  List.iter Adversarial.variants ~f:(fun (label, name) ->
    let subject = Subject.create ~id:"s" ~party:Person ~names:[ name ] () |> ok_exn in
    let score policy =
      match
        (Screening.screen ~policy ~lists:(force lists) ~context:Onboarding ~at subject)
          .decision
      with
      | Clear -> "CLEAR"
      | Potential_match (h :: _) | Confirmed (h :: _) ->
        sprintf "%d %s" h.name.score h.name.best
      | Potential_match [] | Confirmed [] -> assert false
    in
    printf "%-30s %-14s %s\n" label (score Policy.default) (score Policy.payments));
  [%expect {|
    plain                          1000 edit      1000 edit
    Cyrillic homoglyphs            1000 edit      1000 edit
    Cyrillic er for p              1000 edit      1000 edit
    Greek homoglyphs               1000 edit      1000 edit
    surname of look-alikes only    1000 edit      1000 edit
    zero-width space               1000 edit      1000 edit
    zero-width joiners             1000 edit      1000 edit
    soft hyphen                    1000 edit      1000 edit
    word joiner and BOM            1000 edit      1000 edit
    bidi controls                  1000 edit      1000 edit
    variation selector             1000 edit      1000 edit
    fullwidth                      1000 edit      1000 edit
    mathematical bold              1000 edit      1000 edit
    diacritics                     1000 edit      1000 edit
    combining marks                1000 edit      1000 edit
    Cyrillic script                1000 edit      1000 edit
    upper case and comma           1000 edit      1000 edit
    dotted letters                 1000 compact   1000 compact
    hyphenated                     1000 compact   1000 compact
    split word                     1000 compact   1000 compact
    extra spaces and tabs          1000 edit      1000 edit
    digits for letters             1000 edit      1000 edit
    honorific                      1000 edit      1000 edit
    inserted Georgian letter       975 tokens     975 tokens
    |}]
;;
