open! Core

module Name = struct
  type t =
    { entry : int
    ; name : Entry.Name.t
    ; prepared : Match.Prepared.t
    }
end

type t =
  { entries : Entry.t array
  ; names : Name.t array
  ; plain_grams : int array (** per name *)
  ; skeleton_grams : int array (** per name *)
  ; postings : (int, int array) Hashtbl.t (** plain and skeleton trigrams, all names *)
  ; abjad_postings : (int, int array) Hashtbl.t
  (** skeleton trigrams of names written in Arabic script only *)
  ; abjad : bool array (** per name *)
  ; counts : int array
  ; skeleton_counts : int array
  ; touched : int array
  ; mutable n_touched : int
  }

(* Skeleton trigrams live in their own key space. *)
let skeleton_bit = 1 lsl 30

let grams_of_string ~bit (s : string) acc =
  let n = String.length s in
  (* Boundary marks \001 and \002 make the first and last letters count. *)
  let at i = if i < 0 then 1 else if i >= n then 2 else Char.to_int s.[i] in
  for i = -1 to n - 2 do
    acc := (bit lor (at i lsl 16) lor (at (i + 1) lsl 8) lor at (i + 2)) :: !acc
  done
;;

let plain_grams (k : Normalize.Key.t) =
  let acc = ref [] in
  Array.iter k.tokens ~f:(fun t -> grams_of_string ~bit:0 t acc);
  grams_of_string ~bit:0 k.compact acc;
  List.dedup_and_sort !acc ~compare:Int.compare
;;

let skeleton_grams (k : Normalize.Key.t) =
  let acc = ref [] in
  List.iter (String.split k.skeleton ~on:' ') ~f:(fun t ->
    if not (String.is_empty t) then grams_of_string ~bit:skeleton_bit t acc);
  List.dedup_and_sort !acc ~compare:Int.compare
;;

let create entry_list =
  let entries = Array.of_list entry_list in
  let names = Queue.create () in
  Array.iteri entries ~f:(fun i (e : Entry.t) ->
    List.iter e.names ~f:(fun n ->
      List.iter (Normalize.keys ~party:e.party n.name) ~f:(fun key ->
        if not (Normalize.Key.is_empty key)
        then
          Queue.enqueue names { Name.entry = i; name = n; prepared = Match.Prepared.of_key key })));
  let names = Queue.to_array names in
  let lists = Int.Table.create () in
  let abjad_lists = Int.Table.create () in
  let plain = Array.create ~len:(Array.length names) 0 in
  let skel = Array.create ~len:(Array.length names) 0 in
  Array.iteri names ~f:(fun id (n : Name.t) ->
    let p = plain_grams n.prepared.key
    and s = skeleton_grams n.prepared.key in
    plain.(id) <- List.length p;
    skel.(id) <- List.length s;
    List.iter (p @ s) ~f:(fun g -> Hashtbl.add_multi lists ~key:g ~data:id);
    if n.prepared.key.abjad
    then List.iter s ~f:(fun g -> Hashtbl.add_multi abjad_lists ~key:g ~data:id));
  let freeze lists =
    let postings = Int.Table.create ~size:(Hashtbl.length lists) () in
    Hashtbl.iteri lists ~f:(fun ~key ~data ->
      Hashtbl.set postings ~key ~data:(Array.of_list_rev data));
    postings
  in
  { entries
  ; names
  ; plain_grams = plain
  ; skeleton_grams = skel
  ; postings = freeze lists
  ; abjad_postings = freeze abjad_lists
  ; abjad = Array.map names ~f:(fun (n : Name.t) -> n.prepared.key.abjad)
  ; counts = Array.create ~len:(Array.length names) 0
  ; skeleton_counts = Array.create ~len:(Array.length names) 0
  ; touched = Array.create ~len:(Array.length names) 0
  ; n_touched = 0
  }
;;

let entries t = t.entries
let name_count t = Array.length t.names
let name t i = t.names.(i)

let stats t =
  let lengths = Hashtbl.data t.postings |> List.map ~f:Array.length in
  [%sexp
    { entries = (Array.length t.entries : int)
    ; names = (Array.length t.names : int)
    ; trigrams = (Hashtbl.length t.postings : int)
    ; longest_posting = (List.max_elt lengths ~compare:Int.compare |> Option.value ~default:0 : int)
    ; postings_total = (List.sum (module Int) lengths ~f:Fn.id : int)
    }]
;;

let sort_results r =
  List.sort r ~compare:(fun (a, sa) (b, sb) ->
    match Int.compare sb sa with
    | 0 -> Int.compare a b
    | c -> c)
;;

(* Skeleton trigrams take part when either side was written in Arabic script: an Arabic
   query is compared with every name's skeleton, a Latin query with the skeletons of the
   Arabic-script names only. They are counted apart from the plain trigrams so that the
   Dice overlap of two Latin names is not changed by them. *)
let search t scratch ~weights ~prefilter ~threshold (query : Match.Prepared.t) =
  let q_plain = plain_grams query.key in
  let q_skel = skeleton_grams query.key in
  let qp = List.length q_plain
  and qs = List.length q_skel in
  t.n_touched <- 0;
  let bump counts g postings =
    match Hashtbl.find postings g with
    | None -> ()
    | Some ids ->
      for k = 0 to Array.length ids - 1 do
        let id = Array.unsafe_get ids k in
        if t.counts.(id) = 0 && t.skeleton_counts.(id) = 0
        then (
          t.touched.(t.n_touched) <- id;
          t.n_touched <- t.n_touched + 1);
        counts.(id) <- counts.(id) + 1
      done
  in
  List.iter q_plain ~f:(fun g -> bump t.counts g t.postings);
  List.iter q_skel ~f:(fun g ->
    bump t.skeleton_counts g (if query.key.abjad then t.postings else t.abjad_postings));
  let results = ref [] in
  for k = 0 to t.n_touched - 1 do
    let id = t.touched.(k) in
    let use_skeleton = query.key.abjad || t.abjad.(id) in
    let shared = t.counts.(id) + if use_skeleton then t.skeleton_counts.(id) else 0 in
    t.counts.(id) <- 0;
    t.skeleton_counts.(id) <- 0;
    let qn = qp + if use_skeleton then qs else 0 in
    let nn = t.plain_grams.(id) + if use_skeleton then t.skeleton_grams.(id) else 0 in
    (* Dice overlap in per mille, without division. *)
    if 2000 * shared >= prefilter * (qn + nn)
    then (
      let s = Match.score scratch weights query t.names.(id).prepared in
      if s >= threshold then results := (id, s) :: !results)
  done;
  t.n_touched <- 0;
  sort_results !results
;;

let scan t scratch ~weights ~threshold query =
  let results = ref [] in
  Array.iteri t.names ~f:(fun id (n : Name.t) ->
    let s = Match.score scratch weights query n.prepared in
    if s >= threshold then results := (id, s) :: !results);
  sort_results !results
;;
