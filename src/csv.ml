open! Core

let parse ?(sep = ',') text =
  let text = Option.value (String.chop_prefix text ~prefix:"\xEF\xBB\xBF") ~default:text in
  let n = String.length text in
  let rows = ref [] in
  let row = ref [] in
  let field = Buffer.create 64 in
  let end_field () =
    row := Buffer.contents field :: !row;
    Buffer.clear field
  in
  let end_row () =
    end_field ();
    (match !row with
     | [ "" ] -> ()
     | r -> rows := List.rev r :: !rows);
    row := []
  in
  let i = ref 0 in
  let in_quotes = ref false in
  while !i < n do
    let c = text.[!i] in
    if !in_quotes
    then
      if Char.equal c '"'
      then
        if !i + 1 < n && Char.equal text.[!i + 1] '"'
        then (
          Buffer.add_char field '"';
          i := !i + 2)
        else (
          in_quotes := false;
          incr i)
      else (
        Buffer.add_char field c;
        incr i)
    else (
      (match c with
       | '"' when Buffer.length field = 0 -> in_quotes := true
       | '\r' when !i + 1 < n && Char.equal text.[!i + 1] '\n' -> ()
       | '\n' -> end_row ()
       | '\x1a' when !i = n - 1 -> ()
       | c when Char.equal c sep -> end_field ()
       | c -> Buffer.add_char field c);
      incr i)
  done;
  if Buffer.length field > 0 || not (List.is_empty !row) then end_row ();
  List.rev !rows
;;
