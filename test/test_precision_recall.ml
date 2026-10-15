open! Core
open Sanctions_screening
open Fixture_lists

(* Precision and recall on test/labelled.tsv: spellings of fixture entries (positives) and
   other names (negatives). The README quotes these numbers. *)

type row =
  { query : string
  ; party : Party_type.t
  ; expected : String.Set.t
  ; note : string
  }

let rows =
  lazy
    (In_channel.read_lines "labelled.tsv"
     |> List.filter ~f:(fun l ->
       not (String.is_empty l || String.is_prefix l ~prefix:"#"))
     |> List.map ~f:(fun line ->
       match String.split line ~on:'\t' with
       | [ query; party; expected; note ] ->
         { query
         ; party = Party_type.of_string party |> ok_exn
         ; expected =
             (if String.equal expected "-"
              then String.Set.empty
              else String.Set.of_list (String.split expected ~on:','))
         ; note
         }
       | _ -> failwithf "bad labelled row: %s" line ()))
;;

type counts =
  { mutable positives : int
  ; mutable found : int (** positive queries with at least one expected hit *)
  ; mutable expected_keys : int
  ; mutable found_keys : int
  ; mutable negatives : int
  ; mutable false_alerts : int (** negative queries with any hit *)
  ; mutable hits : int
  ; mutable correct_hits : int
  }

let evaluate policy ~on_miss =
  let c =
    { positives = 0
    ; found = 0
    ; expected_keys = 0
    ; found_keys = 0
    ; negatives = 0
    ; false_alerts = 0
    ; hits = 0
    ; correct_hits = 0
    }
  in
  List.iter (force rows) ~f:(fun r ->
    let subject = Subject.create ~id:"q" ~party:r.party ~names:[ r.query ] () |> ok_exn in
    let result = screen ~policy subject in
    let keys = Screening.Result.hit_keys result in
    c.hits <- c.hits + Set.length keys;
    c.correct_hits <- c.correct_hits + Set.length (Set.inter keys r.expected);
    if Set.is_empty r.expected
    then (
      c.negatives <- c.negatives + 1;
      if not (Set.is_empty keys)
      then (
        c.false_alerts <- c.false_alerts + 1;
        on_miss
          (sprintf
             "false alert: %s (%s) -> %s"
             r.query
             r.note
             (String.concat ~sep:"," (Set.to_list keys)))))
    else (
      c.positives <- c.positives + 1;
      c.expected_keys <- c.expected_keys + Set.length r.expected;
      c.found_keys <- c.found_keys + Set.length (Set.inter keys r.expected);
      if Set.is_empty (Set.inter keys r.expected)
      then on_miss (sprintf "missed: %s (%s)" r.query r.note)
      else c.found <- c.found + 1));
  c
;;

let pct a b = if b = 0 then 0. else 100. *. Float.of_int a /. Float.of_int b

let%expect_test "precision and recall by threshold" =
  print_endline "threshold  recall(queries)  recall(entries)  false alerts  hit precision";
  List.iter [ 800; 825; 850; 875; 900 ] ~f:(fun t ->
    let c = evaluate { Policy.default with name_threshold = t } ~on_miss:ignore in
    printf
      "%9d  %6.1f%% %3d/%-3d  %6.1f%% %3d/%-3d  %5.1f%% %2d/%-2d  %6.1f%%\n"
      t
      (pct c.found c.positives)
      c.found
      c.positives
      (pct c.found_keys c.expected_keys)
      c.found_keys
      c.expected_keys
      (pct c.false_alerts c.negatives)
      c.false_alerts
      c.negatives
      (pct c.correct_hits c.hits));
  [%expect
    {|
    threshold  recall(queries)  recall(entries)  false alerts  hit precision
          800   100.0%  84/84     98.7% 155/157   11.4%  5/44    95.1%
          825   100.0%  84/84     97.5% 153/157    2.3%  1/44    98.7%
          850   100.0%  84/84     97.5% 153/157    2.3%  1/44    98.7%
          875    96.4%  81/84     94.9% 149/157    2.3%  1/44    98.7%
          900    96.4%  81/84     92.4% 145/157    2.3%  1/44    98.6%
    |}]
;;

let%expect_test "misses and false alerts at the default policy" =
  ignore (evaluate Policy.default ~on_miss:print_endline : counts);
  [%expect
    {| false alert: Nikolai Belov (surname prefix) -> eu-consolidated:7009,ofac-sdn:9014 |}]
;;
