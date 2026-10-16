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

(* The input: the text not yet consumed, and where more comes from. A whole string is a
   source that is already at its end; a channel source holds one token plus one chunk at a
   time, so a 130 MB file never sits in memory as one string. *)
type source =
  { mutable text : string
  ; mutable pos : int
  ; mutable eof : bool
  ; next_chunk : unit -> string option
  }

let of_string text = { text; pos = 0; eof = true; next_chunk = (fun () -> None) }

(* Drops what was consumed and appends the next chunk; false at the end of the input. *)
let more src =
  if src.eof
  then false
  else (
    match src.next_chunk () with
    | None ->
      src.eof <- true;
      false
    | Some chunk ->
      src.text <- String.drop_prefix src.text src.pos ^ chunk;
      src.pos <- 0;
      true)
;;

(* At least [k] bytes from [pos], or what is left at the end. *)
let rec ensure src k =
  if String.length src.text - src.pos < k && more src then ensure src k
;;

(* The index of [sub] at or after [pos], reading more input as needed. *)
let rec find_from src sub =
  match String.substr_index src.text ~pos:src.pos ~pattern:sub with
  | Some i -> i
  | None -> if more src then find_from src sub else raise (Bad ("expected " ^ sub))
;;

(* The [>] that ends the tag starting at [pos], outside quoted attribute values. *)
let tag_end src =
  (* [more] moves the unconsumed text to the front, so positions are kept relative to
     [pos] across refills. *)
  let rec from offset quote =
    let i = src.pos + offset in
    if i >= String.length src.text
    then if more src then from offset quote else raise (Bad "unterminated tag")
    else (
      let c = src.text.[i] in
      match quote with
      | Some q -> from (offset + 1) (if Char.equal c q then None else quote)
      | None ->
        if Char.equal c '>'
        then i
        else if Char.equal c '"' || Char.equal c '\''
        then from (offset + 1) (Some c)
        else from (offset + 1) None)
  in
  from 1 None
;;

(* A pull parser. Events are delivered to [on_open], [on_close] and [on_text]; callers
   build trees from them. Each token is read whole into [src.text] before it is parsed.
   [wants_text] says whether text here would be kept; text that would not is skipped
   without copying it (the whitespace between the parties of a 130 MB list, mostly). *)
let scan src ~on_open ~on_close ~on_text ~wants_text =
  let is_space c = Char.is_whitespace c in
  let read_name text ~pos ~stop =
    let start = !pos in
    while
      !pos < stop
      && (not (is_space text.[!pos]))
      && not (List.mem [ '>'; '/'; '=' ] text.[!pos] ~equal:Char.equal)
    do
      incr pos
    done;
    if !pos = start then raise (Bad "expected a name");
    String.sub text ~pos:start ~len:(!pos - start)
  in
  let skip_spaces text ~pos ~stop =
    while !pos < stop && is_space text.[!pos] do
      incr pos
    done
  in
  let at substring = String.is_substring_at src.text ~pos:src.pos ~substring in
  let rec loop () =
    ensure src 1;
    if src.pos < String.length src.text
    then (
      if Char.equal src.text.[src.pos] '<'
      then (
        ensure src 9;
        if at "<!--"
        then src.pos <- find_from src "-->" + 3
        else if at "<![CDATA["
        then (
          src.pos <- src.pos + 9;
          let stop = find_from src "]]>" in
          if wants_text ()
          then on_text (String.sub src.text ~pos:src.pos ~len:(stop - src.pos));
          src.pos <- stop + 3)
        else if at "<?"
        then src.pos <- find_from src "?>" + 2
        else if at "<!"
        then src.pos <- find_from src ">" + 1
        else (
          let stop = tag_end src in
          let text = src.text in
          let pos = ref (src.pos + 1) in
          if Char.equal text.[!pos] '/'
          then (
            incr pos;
            let name = read_name text ~pos ~stop in
            skip_spaces text ~pos ~stop;
            if !pos <> stop then raise (Bad "expected >");
            src.pos <- stop + 1;
            on_close (local_name name))
          else (
            let name = local_name (read_name text ~pos ~stop) in
            let attrs = ref [] in
            let fin = ref false in
            while not !fin do
              skip_spaces text ~pos ~stop;
              if !pos >= stop
              then (
                on_open name (List.rev !attrs);
                fin := true)
              else if Char.equal text.[!pos] '/'
              then (
                on_open name (List.rev !attrs);
                on_close name;
                fin := true)
              else (
                let an = local_name (read_name text ~pos ~stop) in
                skip_spaces text ~pos ~stop;
                if !pos >= stop || not (Char.equal text.[!pos] '=')
                then raise (Bad "expected =");
                incr pos;
                skip_spaces text ~pos ~stop;
                let q = text.[!pos] in
                if not (Char.equal q '"' || Char.equal q '\'')
                then raise (Bad "expected quote");
                incr pos;
                let start = !pos in
                let close =
                  match String.index_from text start q with
                  | Some i when i < stop -> i
                  | _ -> raise (Bad "unterminated attribute")
                in
                attrs
                := (an, decode_entities (String.sub text ~pos:start ~len:(close - start)))
                   :: !attrs;
                pos := close + 1)
            done;
            src.pos <- stop + 1)))
      else (
        let rec text_end () =
          match String.index_from src.text src.pos '<' with
          | Some i -> i
          | None -> if more src then text_end () else String.length src.text
        in
        let stop = text_end () in
        if wants_text ()
        then
          on_text
            (decode_entities (String.sub src.text ~pos:src.pos ~len:(stop - src.pos)));
        src.pos <- stop);
      loop ())
  in
  loop ()
;;

(* Builds trees for elements at depth >= [depth]; [emit] receives each finished element at
   exactly [depth]. *)
let run src ~depth ~emit =
  let level = ref 0 in
  (* Stack of open elements being built: name, attrs, reversed children. *)
  let stack : (string * (string * string) list * node list ref) list ref = ref [] in
  try
    scan
      src
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
        | [] -> ())
      ~wants_text:(fun () -> not (List.is_empty !stack));
    if !level <> 0 then raise (Bad "unclosed elements at end of input");
    Ok ()
  with
  | Bad msg -> Or_error.error_s [%message "malformed XML" msg]
;;

let iter_elements text ~depth ~f = run (of_string text) ~depth ~emit:f

let iter_elements_channel ?(chunk_size = 1 lsl 20) ic ~depth ~f =
  let buf = Bytes.create chunk_size in
  let next_chunk () =
    match In_channel.input ic ~buf ~pos:0 ~len:chunk_size with
    | 0 -> None
    | n -> Some (Bytes.To_string.sub buf ~pos:0 ~len:n)
  in
  run { text = ""; pos = 0; eof = false; next_chunk } ~depth ~emit:f
;;

let parse text =
  let root = ref None in
  match run (of_string text) ~depth:0 ~emit:(fun el -> root := Some el) with
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
