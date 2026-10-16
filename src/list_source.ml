open! Core

module Format = struct
  type t =
    | Ofac_sdn_csv
    | Ofac_advanced_xml
    | Eu_xml
    | Uk_ofsi_csv
    | Fixture
  [@@deriving sexp, compare, equal, enumerate]
end

let detect ~filename ~contents : Format.t option =
  let base = String.lowercase (Filename.basename filename) in
  let head = String.prefix contents 4096 in
  if String.is_suffix base ~suffix:".sexp"
  then Some Fixture
  else if String.is_suffix base ~suffix:".xml"
  then
    if String.is_substring head ~substring:"ADVANCED_XML"
    then Some Ofac_advanced_xml
    else if String.is_substring head ~substring:"eu.europa.ec/fpi/fsd/export"
    then Some Eu_xml
    else None
  else if String.is_suffix base ~suffix:".csv"
  then
    if String.is_prefix
         (Option.value (String.chop_prefix head ~prefix:"\xEF\xBB\xBF") ~default:head)
         ~prefix:"Last Updated"
    then Some Uk_ofsi_csv
    else if String.equal base "sdn.csv"
    then Some Ofac_sdn_csv
    else None
  else None
;;

let parse (format : Format.t) ?(version = "unknown") ?alt contents =
  match format with
  | Ofac_sdn_csv -> Ofac_csv.parse ~version ~sdn:contents ?alt ()
  | Ofac_advanced_xml -> Ofac_xml.parse contents
  | Eu_xml -> Eu_xml.parse contents
  | Uk_ofsi_csv -> Uk_csv.parse contents
  | Fixture -> Fixture.parse contents
;;

let rec files dir =
  Sys_unix.readdir dir
  |> Array.to_list
  |> List.sort ~compare:String.compare
  |> List.concat_map ~f:(fun name ->
    let path = Filename.concat dir name in
    match Sys_unix.is_directory path with
    | `Yes -> files path
    | `No | `Unknown -> [ path ])
;;

let first_line contents =
  match String.split_lines contents with
  | first :: _ -> String.strip first
  | [] -> "unknown"
;;

(* The shared core of [load_dir] and [load_files]: [head] gives enough of a file to
   recognise it (at least the 4 KiB [detect] looks at), [read] its contents, [ofac_xml]
   parses an OFAC advanced XML file (from a channel, for files on disk),
   [sibling path name] finds a file of that (lower-case) name in the same directory, and
   [undated path] versions an OFAC CSV that has no VERSION file. *)
let load ~paths ~head ~read ~ofac_xml ~sibling ~undated =
  Or_error.try_with_join (fun () ->
    paths
    |> List.filter_map ~f:(fun path ->
      match detect ~filename:path ~contents:(head path) with
      | None -> None
      | Some Ofac_advanced_xml -> Some (ofac_xml path |> Or_error.tag ~tag:path)
      | Some Ofac_sdn_csv ->
        let alt = Option.map (sibling path "alt.csv") ~f:read in
        let version =
          match sibling path "version" with
          | Some v -> first_line (read v)
          | None -> undated path
        in
        Some (parse Ofac_sdn_csv ~version ?alt (read path) |> Or_error.tag ~tag:path)
      | Some format -> Some (parse format (read path) |> Or_error.tag ~tag:path))
    |> Or_error.combine_errors)
;;

(* One allocation of the file's size. [In_channel.read_all] grows a buffer by doubling and
   copies it out, which for the 130 MB OFAC XML held about three times the file at its
   peak. *)
let read_file path =
  In_channel.with_file path ~binary:true ~f:(fun ic ->
    let len = Int64.to_int_exn (In_channel.length ic) in
    let s = Stdlib.really_input_string ic len in
    (* A file that grew while we read is read again in full rather than cut short. *)
    match In_channel.input_char ic with
    | None -> s
    | Some _ -> In_channel.read_all path)
;;

let load_dir dir =
  let sibling path name =
    let dir = Filename.dirname path in
    Sys_unix.readdir dir
    |> Array.to_list
    |> List.find ~f:(fun f -> String.equal (String.lowercase f) name)
    |> Option.map ~f:(Filename.concat dir)
  in
  let undated path =
    let mtime = (Core_unix.stat path).st_mtime in
    Date.to_string
      (Date.of_time
         (Time_float.of_span_since_epoch (Time_float.Span.of_sec mtime))
         ~zone:Time_float.Zone.utc)
  in
  Or_error.try_with_join (fun () ->
    load
      ~paths:(files dir)
      ~head:(fun path ->
        In_channel.with_file path ~binary:true ~f:(fun ic ->
          let buf = Bytes.create 4096 in
          let n = In_channel.input ic ~buf ~pos:0 ~len:4096 in
          Bytes.To_string.sub buf ~pos:0 ~len:n))
      ~read:read_file
      ~ofac_xml:(fun path ->
        In_channel.with_file path ~binary:true ~f:Ofac_xml.of_channel)
      ~sibling
      ~undated)
;;

(* Path order as [load_dir] walks a tree: component by component. *)
let compare_paths a b =
  List.compare String.compare (String.split a ~on:'/') (String.split b ~on:'/')
;;

let load_files files =
  match
    String.Map.of_alist (List.map files ~f:(fun (path, contents) -> path, contents))
  with
  | `Duplicate_key path -> Or_error.error_s [%message "file given twice" path]
  | `Ok by_path ->
    let sibling path name =
      let dir = Filename.dirname path in
      Map.keys by_path
      |> List.find ~f:(fun p ->
        String.equal (Filename.dirname p) dir
        && String.equal (String.lowercase (Filename.basename p)) name)
    in
    load
      ~paths:(List.sort (Map.keys by_path) ~compare:compare_paths)
      ~head:(Map.find_exn by_path)
      ~read:(Map.find_exn by_path)
      ~ofac_xml:(fun path -> Ofac_xml.parse (Map.find_exn by_path path))
      ~sibling
      ~undated:(fun _ -> "unknown")
;;

let fixture_files () = Bundled_fixtures.files
let load_fixtures () = load_files Bundled_fixtures.files
