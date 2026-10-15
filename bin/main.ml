open! Core
open Sanctions_screening

let load dir =
  let snapshots = List_source.load_dir dir |> ok_exn in
  Lists.create snapshots |> ok_exn
;;

let lists_flag name =
  Command.Param.flag name (Command.Param.required Command.Param.string) ~doc:"DIR list files"
;;

let at_flag =
  Command.Param.flag
    "at"
    (Command.Param.optional Command.Param.string)
    ~doc:"TIME screening time (default now; fix it for reproducible ids)"
;;

let time_of = function
  | None -> Time_ns.now ()
  | Some s -> Time_ns.of_string_with_utc_offset s
;;

let policy_of ~payments ~threshold =
  let base = if payments then Policy.payments else Policy.default in
  match threshold with
  | None -> base
  | Some name_threshold -> { base with name_threshold }
;;

let print_hit ~explain (h : Hit.t) =
  print_endline ("  " ^ Hit.to_string_hum h);
  if explain
  then (
    printf "    programmes: %s\n" (String.concat ~sep:", " h.programmes);
    print_endline
      ("    " ^ Sexp.to_string [%sexp (h.name.components : Match.Components.t)]))
;;

let screen_command =
  Command.basic
    ~summary:"screen one subject"
    (let%map_open.Command names =
       flag "name" (one_or_more_as_list string) ~doc:"NAME a name of the subject (repeatable)"
     and dob = flag "dob" (optional string) ~doc:"DATE 1961-03-12, 1961-03 or 1961"
     and party =
       flag
         "type"
         (optional_with_default "unknown" string)
         ~doc:"TYPE person, entity, vessel, aircraft or unknown"
     and nationalities =
       flag "nationality" (listed string) ~doc:"CC ISO country code (repeatable)"
     and dir = lists_flag "lists"
     and payments = flag "payments" no_arg ~doc:" use the payment counterparty policy"
     and threshold = flag "threshold" (optional int) ~doc:"N name threshold, 0 to 1000"
     and explain = flag "explain" no_arg ~doc:" print every score component"
     and at = at_flag in
     fun () ->
       let subject =
         Subject.create
           ~id:"cli"
           ~party:(Party_type.of_string party |> ok_exn)
           ~names
           ?dob:(Option.map dob ~f:(fun d -> Dob.of_string d |> ok_exn))
           ~nationalities
           ()
         |> ok_exn
       in
       let policy = policy_of ~payments ~threshold in
       let r =
         Screening.screen
           ~policy
           ~lists:(load dir)
           ~context:Onboarding
           ~at:(time_of at)
           subject
       in
       match r.decision with
       | Clear -> print_endline "clear"
       | Potential_match hits | Confirmed hits ->
         print_endline
           (match r.decision with
            | Confirmed _ -> "confirmed"
            | _ -> "potential match");
         List.iter hits ~f:(print_hit ~explain))
;;

let rescreen_command =
  Command.basic
    ~summary:"rescreen subjects after a list update"
    ~readme:(fun () ->
      "SUBJECTS is a file of subject s-expressions, one per line or as a list:\n\
       ((id c-1) (party Person) (names (\"Oleg Paramonov\")) (dob ((Year 1961))) \
       (nationalities (RU)))")
    (let%map_open.Command subjects = anon ("SUBJECTS" %: string)
     and before = lists_flag "before"
     and after = lists_flag "after"
     and payments = flag "payments" no_arg ~doc:" use the payment counterparty policy"
     and at = at_flag in
     fun () ->
       let policy = policy_of ~payments ~threshold:None in
       let at = time_of at in
       let before = load before
       and after = load after in
       let subjects = Sexp.load_sexps_conv_exn subjects Subject.t_of_sexp in
       let population =
         List.map subjects ~f:(fun s ->
           s, (Screening.screen ~policy ~lists:before ~context:Periodic ~at s).decision)
       in
       let r = Screening.rescreen ~policy ~before ~after ~at population |> ok_exn in
       List.iter r.diff ~f:(fun (list, d) ->
         printf
           "%s: %d added, %d removed, %d changed\n"
           (List_id.to_string list)
           (List.length d.added)
           (List.length d.removed)
           (List.length d.changed));
       printf "rescreened %d, unchanged %d\n" r.rescreened r.unchanged;
       List.iter r.newly_hit ~f:(fun c ->
         printf "newly hit: %s\n" c.subject.id;
         List.iter (Decision.hits c.result.decision) ~f:(print_hit ~explain:false));
       List.iter r.cleared ~f:(fun c -> printf "cleared: %s\n" c.subject.id))
;;

let stats_command =
  Command.basic
    ~summary:"describe the lists in a directory"
    (let%map_open.Command dir = lists_flag "lists"
     and timing = flag "time" no_arg ~doc:" also print load and index times" in
     fun () ->
       let t0 = Time_ns.now () in
       let snapshots = List_source.load_dir dir |> ok_exn in
       let t1 = Time_ns.now () in
       let lists = Lists.create snapshots |> ok_exn in
       let t2 = Time_ns.now () in
       List.iter snapshots ~f:(fun s ->
         let count p =
           List.count s.entries ~f:(fun (e : Entry.t) -> Party_type.equal e.party p)
         in
         printf
           "%s %s: %d entries (%s), %d names, digest %s\n"
           (List_id.to_string s.version.list)
           s.version.version
           s.version.entries
           (List.filter_map Party_type.all ~f:(fun p ->
              match count p with
              | 0 -> None
              | n -> Some (sprintf "%d %s" n (Party_type.to_string p)))
            |> String.concat ~sep:", ")
           (List.sum (module Int) s.entries ~f:(fun e -> List.length e.names))
           (String.prefix s.version.digest 12));
       print_s (Index.stats (Lists.index lists));
       if timing
       then
         printf
           "load %s, index %s\n"
           (Time_ns.Span.to_string_hum (Time_ns.diff t1 t0))
           (Time_ns.Span.to_string_hum (Time_ns.diff t2 t1)))
;;

let () =
  Command.group
    ~summary:"Name screening against sanctions lists"
    [ "screen", screen_command; "rescreen", rescreen_command; "stats", stats_command ]
  |> Command_unix.run
;;
