open! Core

let replacement = (0xFFFD lsl 3) lor 1
let[@inline] byte s i = Char.to_int (String.unsafe_get s i)
let[@inline] cont b = b land 0xC0 = 0x80

let[@zero_alloc] decode s ~pos =
  let n = String.length s in
  let b0 = byte s pos in
  if b0 < 0x80
  then (b0 lsl 3) lor 1
  else if b0 land 0xE0 = 0xC0
  then
    if pos + 1 < n && cont (byte s (pos + 1))
    then (
      let cp = ((b0 land 0x1F) lsl 6) lor (byte s (pos + 1) land 0x3F) in
      if cp < 0x80 then replacement else (cp lsl 3) lor 2)
    else replacement
  else if b0 land 0xF0 = 0xE0
  then
    if pos + 2 < n && cont (byte s (pos + 1)) && cont (byte s (pos + 2))
    then (
      let cp =
        ((b0 land 0x0F) lsl 12)
        lor ((byte s (pos + 1) land 0x3F) lsl 6)
        lor (byte s (pos + 2) land 0x3F)
      in
      if cp < 0x800 || (cp >= 0xD800 && cp <= 0xDFFF) then replacement else (cp lsl 3) lor 3)
    else replacement
  else if b0 land 0xF8 = 0xF0
  then
    if pos + 3 < n
       && cont (byte s (pos + 1))
       && cont (byte s (pos + 2))
       && cont (byte s (pos + 3))
    then (
      let cp =
        ((b0 land 0x07) lsl 18)
        lor ((byte s (pos + 1) land 0x3F) lsl 12)
        lor ((byte s (pos + 2) land 0x3F) lsl 6)
        lor (byte s (pos + 3) land 0x3F)
      in
      if cp < 0x10000 || cp > 0x10FFFF then replacement else (cp lsl 3) lor 4)
    else replacement
  else replacement
;;

let[@zero_alloc] code_point packed = packed lsr 3
let[@zero_alloc] length packed = packed land 7

let encode buf cp =
  let add i = Buffer.add_char buf (Char.unsafe_of_int i) in
  if cp < 0
  then Buffer.add_string buf "\xEF\xBF\xBD"
  else if cp < 0x80
  then add cp
  else if cp < 0x800
  then (
    add (0xC0 lor (cp lsr 6));
    add (0x80 lor (cp land 0x3F)))
  else if cp < 0x10000
  then
    if cp >= 0xD800 && cp <= 0xDFFF
    then Buffer.add_string buf "\xEF\xBF\xBD"
    else (
      add (0xE0 lor (cp lsr 12));
      add (0x80 lor ((cp lsr 6) land 0x3F));
      add (0x80 lor (cp land 0x3F)))
  else if cp <= 0x10FFFF
  then (
    add (0xF0 lor (cp lsr 18));
    add (0x80 lor ((cp lsr 12) land 0x3F));
    add (0x80 lor ((cp lsr 6) land 0x3F));
    add (0x80 lor (cp land 0x3F)))
  else Buffer.add_string buf "\xEF\xBF\xBD"
;;

let iter s ~f =
  let n = String.length s in
  let pos = ref 0 in
  while !pos < n do
    let d = decode s ~pos:!pos in
    f (code_point d);
    pos := !pos + length d
  done
;;
