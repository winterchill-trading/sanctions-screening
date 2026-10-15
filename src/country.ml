open! Core

let table =
  let t = String.Table.create () in
  List.iter Country_names.all ~f:(fun (name, code) ->
    ignore (Hashtbl.add t ~key:(Normalize.fold name) ~data:code : [ `Ok | `Duplicate ]));
  t
;;

let codes = String.Set.of_list (List.map Country_names.all ~f:snd)

let of_name s =
  let s = String.strip s in
  if String.length s = 2 && Set.mem codes (String.uppercase s)
  then Some (String.uppercase s)
  else Hashtbl.find table (Normalize.fold s)
;;
