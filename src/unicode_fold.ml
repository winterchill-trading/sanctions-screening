open! Core

let unicode_version = Unicode_data.unicode_version

(* Index of [cp] in a sorted array, or -1. Plain loop so it stays allocation-free. *)
let[@zero_alloc] find (keys : int array) cp =
  let lo = ref 0
  and hi = ref (Array.length keys - 1)
  and found = ref (-1) in
  while !found < 0 && !lo <= !hi do
    let mid = (!lo + !hi) lsr 1 in
    let k = Array.unsafe_get keys mid in
    if k = cp then found := mid else if k < cp then lo := mid + 1 else hi := mid - 1
  done;
  !found
;;

let decompose cp ~f =
  if cp < 0x80
  then f cp
  else (
    let i = find Unicode_data.decomp_keys cp in
    if i < 0
    then f cp
    else
      for j = Unicode_data.decomp_offsets.(i) to Unicode_data.decomp_offsets.(i + 1) - 1 do
        f Unicode_data.decomp_values.(j)
      done)
;;

let[@zero_alloc] is_mark cp =
  if cp < 0x300
  then false
  else (
    let r = Unicode_data.mark_ranges in
    (* Binary search over the range starts (even indices). *)
    let lo = ref 0
    and hi = ref ((Array.length r / 2) - 1)
    and ans = ref false
    and fin = ref false in
    while (not !fin) && !lo <= !hi do
      let mid = (!lo + !hi) lsr 1 in
      let a = Array.unsafe_get r (2 * mid)
      and b = Array.unsafe_get r ((2 * mid) + 1) in
      if cp < a
      then hi := mid - 1
      else if cp > b
      then lo := mid + 1
      else (
        ans := true;
        fin := true)
    done;
    !ans)
;;

let[@zero_alloc] lower cp =
  if cp < 0x80
  then if cp >= Char.to_int 'A' && cp <= Char.to_int 'Z' then cp + 32 else cp
  else (
    let i = find Unicode_data.lower_keys cp in
    if i < 0 then cp else Array.unsafe_get Unicode_data.lower_values i)
;;

let fold s =
  let buf = Buffer.create (String.length s) in
  Utf8.iter s ~f:(fun cp ->
    decompose cp ~f:(fun d -> if not (is_mark d) then Utf8.encode buf (lower d)));
  Buffer.contents buf
;;
