(* Embeds the fixture lists into the library so List_source.load_fixtures needs no files.
   Arguments are paths under a [fixtures/] directory; prints
   [let files = [ "<path below fixtures/>", "<contents>"; ... ]] in path order. *)

let below_fixtures path =
  let marker = "fixtures/" in
  let n = String.length marker in
  let rec find i =
    if i + n > String.length path
    then failwith ("not under fixtures/: " ^ path)
    else if String.sub path i n = marker
    then String.sub path (i + n) (String.length path - i - n)
    else find (i + 1)
  in
  find 0
;;

let () =
  let files =
    Array.to_list Sys.argv
    |> List.tl
    |> List.filter (fun f -> Filename.basename f <> "dune")
    |> List.map (fun f -> below_fixtures f, f)
    |> List.sort compare
  in
  print_string "let files = [\n";
  List.iter
    (fun (name, path) ->
      let ic = open_in_bin path in
      let contents = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Printf.printf "  %S, %S;\n" name contents)
    files;
  print_string "]\n"
;;
