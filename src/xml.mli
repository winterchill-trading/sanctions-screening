(** A small XML reader for the list files: elements, attributes, text, CDATA, comments,
    processing instructions and the five predefined plus numeric character references. No
    DTDs. Namespace prefixes are dropped from names ([ns:Entry] reads as [Entry]).

    {!iter_elements} streams: it builds the tree of one element at a time at a chosen
    depth, so a 130 MB file never sits in memory as one tree, and {!iter_elements_channel}
    does the same from a channel, so the file never sits in memory as one string either. *)

open! Core

type t =
  { name : string
  ; attrs : (string * string) list
  ; children : node list
  }

and node =
  | Element of t
  | Text of string

(** The whole document's root element. *)
val parse : string -> t Or_error.t

(** Calls [f] with each element at [depth] (the root is depth 0) as a complete tree, and
    [on_text] is not needed: text outside those elements is ignored. *)
val iter_elements : string -> depth:int -> f:(t -> unit) -> unit Or_error.t

(** {!iter_elements} reading from a channel [chunk_size] bytes at a time (default 1 MiB):
    memory holds the current element's tree and about one chunk, not the document. *)
val iter_elements_channel
  :  ?chunk_size:int
  -> In_channel.t
  -> depth:int
  -> f:(t -> unit)
  -> unit Or_error.t

val attr : t -> string -> string option

(** Attribute or [""]. *)
val attr_or_empty : t -> string -> string

(** Child elements with this name. *)
val children : t -> string -> t list

val child : t -> string -> t option

(** Concatenated text of the element and its descendants, stripped. *)
val text : t -> string

(** Elements with this name anywhere below (depth first, document order). *)
val descendants : t -> string -> t list
