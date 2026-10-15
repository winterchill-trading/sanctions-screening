(** Minimal UTF-8 decoding and encoding for the normaliser. *)

(** [decode s ~pos] reads one code point starting at byte [pos] and returns it packed
    as [(code_point lsl 3) lor byte_length]; use {!code_point} and {!length} to unpack.
    Malformed or truncated sequences decode as U+FFFD with length 1, so a scan always
    makes progress. Packing keeps the call allocation-free. *)
val decode : string -> pos:int -> int
[@@zero_alloc]

val code_point : int -> int [@@zero_alloc]
val length : int -> int [@@zero_alloc]

(** Appends the UTF-8 encoding of a code point (U+FFFD for an invalid one). *)
val encode : Buffer.t -> int -> unit

(** Calls [f] with every code point of [s], in order. *)
val iter : string -> f:(int -> unit) -> unit
