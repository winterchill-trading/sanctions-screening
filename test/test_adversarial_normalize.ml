open! Core
open Sanctions_screening

(* Every disguise must give a key that agrees with the listed name, either on the sorted
   words or on the separator-free form. The one exception is a foreign letter inserted
   into a word, which is kept (never deleted) and left to fuzzy matching. *)
let%expect_test "disguised spellings normalise to the listed name" =
  let target = Normalize.key ~party:Person Adversarial.listed in
  List.iter Adversarial.variants ~f:(fun (label, name) ->
    let keys = Normalize.keys ~party:Person name in
    let agrees =
      List.exists keys ~f:(fun (k : Normalize.Key.t) ->
        String.equal k.sorted target.sorted || String.equal k.compact target.compact)
    in
    let shown =
      List.map keys ~f:(fun (k : Normalize.Key.t) -> k.sorted) |> String.concat ~sep:" | "
    in
    printf "%-30s %-5b %s\n" label agrees shown);
  [%expect {|
    plain                          true  oleg paramonov
    Cyrillic homoglyphs            true  oleg paramonov
    Cyrillic er for p              true  oleg raramonov | oleg paramonov
    Greek homoglyphs               true  oleg raramonov | oleg paramonov
    surname of look-alikes only    true  oleg ragamopov | oleg paramonov
    zero-width space               true  oleg paramonov
    zero-width joiners             true  oleg paramonov
    soft hyphen                    true  oleg paramonov
    word joiner and BOM            true  oleg paramonov
    bidi controls                  true  oleg paramonov
    variation selector             true  oleg paramonov
    fullwidth                      true  oleg paramonov
    mathematical bold              true  oleg paramonov
    diacritics                     true  oleg paramonov
    combining marks                true  oleg paramonov
    Cyrillic script                true  oleg paramonov | napamohob oleg
    upper case and comma           true  oleg paramonov
    dotted letters                 true  a e g l m n o p r v
    hyphenated                     true  monov oleg para
    split word                     true  monov oleg para
    extra spaces and tabs          true  oleg paramonov
    digits for letters             true  0leg param0n0v | oleg paramonov
    honorific                      true  oleg paramonov
    inserted Georgian letter       false oleg paraმmonov
    |}]
;;
