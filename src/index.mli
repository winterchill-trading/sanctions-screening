(** In-memory inverted index over the names of a set of list entries.

    Every name (primary and aliases, in every reading) is normalised once
    ({!Normalize.keys}) and broken into character trigrams: the trigrams of each token
    padded with boundary marks, and of the separator-free form ({!Normalize.Key.compact}),
    so that split or joined words still share most trigrams. Consonant-skeleton trigrams
    are indexed too, and used when the query was written in Arabic script.

    A query counts, per name, the trigrams it shares, and only names whose Dice overlap
    ([2 * shared / (query trigrams + name trigrams)]) reaches the policy's [prefilter]
    are scored with {!Match.score}. The prefilter trades speed for recall; the tests check
    that on the labelled set it loses no hit that a full scan finds.

    An index holds scratch buffers: one query at a time (Async code is single-threaded,
    which is enough). *)

open! Core

type t

module Name : sig
  type t = private
    { entry : int (** index into {!entries} *)
    ; name : Entry.Name.t
    ; prepared : Match.Prepared.t
    }
end

val create : Entry.t list -> t
val entries : t -> Entry.t array
val name_count : t -> int
val name : t -> int -> Name.t

(** Trigram count and posting statistics, for [stats]. *)
val stats : t -> Sexp.t

(** Names whose name score against [query] reaches [threshold], as (name id, score),
    best first. [prefilter] is per mille (0 scores every name). *)
val search
  :  t
  -> Match.Scratch.t
  -> weights:Match.Weights.t
  -> prefilter:int
  -> threshold:int
  -> Match.Prepared.t
  -> (int * int) list

(** The same answer by scoring every name, for tests and for measuring the prefilter. *)
val scan
  :  t
  -> Match.Scratch.t
  -> weights:Match.Weights.t
  -> threshold:int
  -> Match.Prepared.t
  -> (int * int) list
