(** The files under [fixtures/], embedded at build time as (path below [fixtures/],
    contents), in path order. Only {!List_source} reads it. *)

val files : (string * string) list
