open! Core

type t =
  { name : string
  ; attrs : (string * string) list
  ; children : node list
  }

and node =
  | Element of t
  | Text of string

exception Bad of string

let local_name s =
  match String.rindex s ':' with
  | Some i -> String.drop_prefix s (i + 1)
  | None -> s
;;

let decode_entities s =
  if not (String.mem s '&')
  then s
  else (
    let buf = Buffer.create (String.length s) in
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      if Char.equal s.[!i] '&'
      then (
        match String.index_from s !i ';' with
        | None -> raise (Bad "unterminated character reference")
        | Some j ->
          let ent = String.sub s ~pos:(!i + 1) ~len:(j - !i - 1) in
          (match ent with
           | "amp" -> Buffer.add_char buf '&'
           | "lt" -> Buffer.add_char buf '<'
           | "gt" -> Buffer.add_char buf '>'
           | "quot" -> Buffer.add_char buf '"'
           | "apos" -> Buffer.add_char buf '\''
           | _ ->
             let cp =
               match String.chop_prefix ent ~prefix:"#x" with
               | Some hex -> Int.of_string_opt ("0x" ^ hex)
               | None ->
                 (match String.chop_prefix ent ~prefix:"#" with
                  | Some dec -> Int.of_string_opt dec
                  | None -> None)
             in
             (match cp with
              | Some cp -> Utf8.encode buf cp
              | None -> raise (Bad ("unknown entity " ^ ent))));
          i := j + 1)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    Buffer.contents buf)
;;

(* A pull parser over the whole string. Events are delivered to [on_open], [on_close] and
   [on_text]; callers build trees from them. *)
let scan text ~on_open ~on_close ~on_text =
  let n = String.length text in
  let pos = ref 0 in
  let find_from sub =
    match String.substr_index text ~pos:!pos ~pattern:sub with
    | Some i -> i
    | None -> raise (Bad ("expected " ^ sub))
  in
  let is_space c = Char.is_whitespace c in
  let skip_spaces () =
    while !pos < n && is_space text.[!pos] do
      incr pos
    done
  in
  let read_name () =
    let start = !pos in
    while
      !pos < n
      && (not (is_space text.[!pos]))
      && not (List.mem [ '>'; '/'; '=' ] text.[!pos] ~equal:Char.equal)
    do
      incr pos
    done;
    if !pos = start then raise (Bad "expected a name");
    String.sub text ~pos:start ~len:(!pos - start)
  in
  while !pos < n do
    if Char.equal text.[!pos] '<'
    then
      if String.is_substring_at text ~pos:!pos ~substring:"<!--"
      then pos := find_from "-->" + 3
      else if String.is_substring_at text ~pos:!pos ~substring:"<![CDATA["
      then (
        let start = !pos + 9 in
        pos := start;
        let stop = find_from "]]>" in
        on_text (String.sub text ~pos:start ~len:(stop - start));
        pos := stop + 3)
      else if String.is_substring_at text ~pos:!pos ~substring:"<?"
      then pos := find_from "?>" + 2
      else if String.is_substring_at text ~pos:!pos ~substring:"<!"
      then pos := find_from ">" + 1
      else if String.is_substring_at text ~pos:!pos ~substring:"</"
      then (
        pos := !pos + 2;
        let name = read_name () in
        skip_spaces ();
        if !pos >= n || not (Char.equal text.[!pos] '>') then raise (Bad "expected >");
        incr pos;
        on_close (local_name name))
      else (
        incr pos;
        let name = local_name (read_name ()) in
        let attrs = ref [] in
        let fin = ref false in
        while not !fin do
          skip_spaces ();
          if !pos >= n then raise (Bad "unterminated tag");
          match text.[!pos] with
          | '>' ->
            incr pos;
            on_open name (List.rev !attrs);
            fin := true
          | '/' ->
            pos := !pos + 2;
            on_open name (List.rev !attrs);
            on_close name;
            fin := true
          | _ ->
            let an = local_name (read_name ()) in
            skip_spaces ();
            if !pos >= n || not (Char.equal text.[!pos] '=') then raise (Bad "expected =");
            incr pos;
            skip_spaces ();
            let q = text.[!pos] in
            if not (Char.equal q '"' || Char.equal q '\'')
            then raise (Bad "expected quote");
            incr pos;
            let start = !pos in
            let stop =
              match String.index_from text start q with
              | Some i -> i
              | None -> raise (Bad "unterminated attribute")
            in
            attrs
            := (an, decode_entities (String.sub text ~pos:start ~len:(stop - start)))
               :: !attrs;
            pos := stop + 1
        done)
    else (
      let stop =
        match String.index_from text !pos '<' with
        | Some i -> i
        | None -> n
      in
      on_text (decode_entities (String.sub text ~pos:!pos ~len:(stop - !pos)));
      pos := stop)
  done
;;

(* Builds trees for elements at depth >= [depth]; [emit] receives each finished element at
   exactly [depth]. *)
let run text ~depth ~emit =
  let level = ref 0 in
  (* Stack of open elements being built: name, attrs, reversed children. *)
  let stack : (string * (string * string) list * node list ref) list ref = ref [] in
  try
    scan
      text
      ~on_open:(fun name attrs ->
        if !level >= depth then stack := (name, attrs, ref []) :: !stack;
        incr level)
      ~on_close:(fun name ->
        decr level;
        if !level >= depth
        then (
          match !stack with
          | (open_name, attrs, kids) :: rest ->
            if not (String.equal open_name name)
            then raise (Bad (sprintf "</%s> closes <%s>" name open_name));
            let el = { name; attrs; children = List.rev !kids } in
            stack := rest;
            (match rest with
             | (_, _, parent) :: _ when !level > depth -> parent := Element el :: !parent
             | _ -> emit el)
          | [] -> raise (Bad "unbalanced close")))
      ~on_text:(fun s ->
        match !stack with
        | (_, _, kids) :: _ -> kids := Text s :: !kids
        | [] -> ());
    if !level <> 0 then raise (Bad "unclosed elements at end of input");
    Ok ()
  with
  | Bad msg -> Or_error.error_s [%message "malformed XML" msg]
;;

let iter_elements text ~depth ~f = run text ~depth ~emit:f

let parse text =
  let root = ref None in
  match run text ~depth:0 ~emit:(fun el -> root := Some el) with
  | Error _ as e -> e
  | Ok () ->
    (match !root with
     | Some r -> Ok r
     | None -> Or_error.error_s [%message "XML document without a root element"])
;;

let attr t name = List.Assoc.find t.attrs name ~equal:String.equal
let attr_or_empty t name = Option.value (attr t name) ~default:""

let children t name =
  List.filter_map t.children ~f:(function
    | Element e when String.equal e.name name -> Some e
    | Element _ | Text _ -> None)
;;

let child t name = List.hd (children t name)

let text t =
  let buf = Buffer.create 32 in
  let rec go t =
    List.iter t.children ~f:(function
      | Text s -> Buffer.add_string buf s
      | Element e -> go e)
  in
  go t;
  String.strip (Buffer.contents buf)
;;

let descendants t name =
  let out = ref [] in
  let rec go t =
    List.iter t.children ~f:(function
      | Element e ->
        if String.equal e.name name then out := e :: !out;
        go e
      | Text _ -> ())
  in
  go t;
  List.rev !out
;;
