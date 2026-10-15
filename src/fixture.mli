(** A list kept as an s-expression file: internal lists, PEP extracts, test lists.

    {v
      ((list (Other internal-pep))
       (version 2026-10-01)
       (entries
        (((id P-1) (party Person) (names (((name "Jane Example") (kind Primary))))
          (dobs ((Year 1970))) (nationalities (GB)) (programmes (PEP))))))
    v}

    [dobs], [nationalities], [programmes] and [listed_on] may be left out. *)

open! Core

val parse : string -> Snapshot.t Or_error.t

(** Writes a snapshot in this format (any list, not only [Other]). *)
val to_string : Snapshot.t -> string
