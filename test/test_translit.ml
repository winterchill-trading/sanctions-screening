open! Core
open Sanctions_screening

let latinise s =
  let buf = Buffer.create 16 in
  Utf8.iter (Unicode_fold.fold s) ~f:(fun cp ->
    match Translit.latin cp with
    | Some l -> Buffer.add_string buf l
    | None -> Utf8.encode buf cp);
  print_endline (Buffer.contents buf)
;;

let%expect_test "public tables" =
  latinise "Щербаков Юрий Жуков";
  latinise "Ђорђевић Љубица";
  latinise "Ґалина Їжак";
  (* Letter by letter; Normalize applies the ELOT 743 digraph ου -> ou. *)
  latinise "Παπαδόπουλος Χρήστος";
  latinise "محمد عبد الله";
  latinise "Straße Æsir Øystein Łukasz";
  [%expect
    {|
    shcherbakov iurii zhukov
    dordevic ljubitsa
    galina izhak
    papadopoylos christos
    mhmd bd allh
    strasse aesir oystein lukasz
    |}]
;;

let%expect_test "tables do not overlap and only produce lowercase ASCII" =
  let all = Translit.cyrillic @ Translit.greek @ Translit.arabic @ Translit.latin_extra in
  let dups = List.find_all_dups all ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
  print_s [%sexp (dups : (int * string) list)];
  List.iter all ~f:(fun (_, l) ->
    assert (String.for_all l ~f:(fun c -> Char.is_lowercase c || Char.is_digit c)));
  [%expect {| () |}]
;;
