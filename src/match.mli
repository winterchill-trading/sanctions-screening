(** String similarity for names, on normalised ASCII keys ({!Normalize.Key.t}).

    Every score is an integer from 0 to 1000 (per mille), computed with integer arithmetic
    only, so results are identical on every platform and easy to pin in tests and logs.

    The primitive comparisons are allocation-free ([[@zero_alloc]], checked by the
    compiler) and use a {!Scratch.t} that the caller allocates once and reuses. Strings
    longer than {!Scratch.max_length} bytes are compared on their first
    {!Scratch.max_length} bytes. *)

open! Core

module Scratch : sig
  type t

  (** 255 bytes: longer than any list name after normalisation. *)
  val max_length : int

  val create : unit -> t
end

(** Jaro-Winkler similarity (prefix scale 0.1, at most 4 prefix characters, applied when
    the Jaro similarity is above 700). [jaro_winkler s "martha" "marhta"] is 961. *)
val jaro_winkler : Scratch.t -> string -> string -> int
[@@zero_alloc]

(** Damerau-Levenshtein distance, optimal string alignment variant (adjacent
    transpositions count one edit). *)
val damerau_levenshtein : Scratch.t -> string -> string -> int
[@@zero_alloc]

(** [1000 - 1000 * distance / longer length], 1000 for two empty strings. *)
val edit_similarity : Scratch.t -> string -> string -> int
[@@zero_alloc]

(** American Soundex of an ASCII lowercase word packed in an int (first letter's code
    times 1000 plus the three digits): ["robert"] and ["rupert"] are both R163. 0 for a
    word without letters. *)
val soundex : string -> int
[@@zero_alloc]

(** Soundex code as text, e.g. ["R163"]. *)
val soundex_to_string : int -> string

(** Token alignment in the manner of Monge-Elkan: each token of the name with fewer tokens
    (ties broken by comparing the tokens, so the result does not depend on argument order)
    is paired with the most similar unused token of the other by Jaro-Winkler (a
    one-letter token is an initial: 900 against any word with that first letter); pairs
    below [token_floor] count 0. Returns the sum of the pairs' similarities. *)
val token_alignment : Scratch.t -> token_floor:int -> string array -> string array -> int
[@@zero_alloc]

(** Shared Soundex codes of two sorted code arrays (multiset intersection size). *)
val common_codes : int array -> int array -> int
[@@zero_alloc]

(** Per-component scores of one name pair, kept for explanations. *)
module Components : sig
  type t =
    { jaro_winkler : int
    (** on the sorted-token strings; reported, not used in the score, because a shared
        first word lifts unrelated names too much *)
    ; edit : int (** {!edit_similarity} on the sorted-token strings *)
    ; compact : int (** {!edit_similarity} on the separator-free strings *)
    ; token_set : int (** {!token_alignment} over the shorter name's token count *)
    ; token_cover : int
    (** {!token_alignment} over the longer name's token count, not counting its unmatched
        patronymics (["-ovich"], ["-evna"], ["-yovych"]): a customer often leaves the
        patronymic out *)
    ; phonetic : int (** shared Soundex codes over the longer name's token count *)
    ; skeleton : int
    (** Jaro-Winkler on consonant skeletons, when either side was written in Arabic
        script; 0 otherwise *)
    }
  [@@deriving sexp, compare, equal, bin_io]
end

(** How components combine into a name score. *)
module Weights : sig
  type t =
    { token_set_share : int
    (** per mille of [token_set] in the token score; the rest is [token_cover] *)
    ; phonetic_cap : int
    (** a full phonetic agreement scores at most this, since Soundex alone is weak *)
    ; skeleton_cap : int
    ; token_floor : int (** token pairs less similar than this count 0 *)
    }
  [@@deriving sexp, compare, equal, bin_io]

  (** [{ token_set_share = 600; phonetic_cap = 880; skeleton_cap = 930; token_floor = 850 }],
      tuned on the labelled set in [test/]. *)
  val default : t
end

module Name_score : sig
  type t =
    { score : int
    ; best : string (** the component that gave [score] *)
    ; components : Components.t
    }
  [@@deriving sexp, compare, equal, bin_io]
end

(** A prepared key: {!Normalize.Key.t} plus its sorted Soundex codes. *)
module Prepared : sig
  type t = private
    { key : Normalize.Key.t
    ; codes : int array
    }

  val of_key : Normalize.Key.t -> t
end

(** The name score of two prepared keys: the maximum of [edit], [compact], the token score
    ([token_set * share + token_cover * (1000 - share)]), the capped phonetic score and
    the capped skeleton score. Symmetric in its two arguments. *)
val score : Scratch.t -> Weights.t -> Prepared.t -> Prepared.t -> int
[@@zero_alloc]

(** Same as {!score}, with the components. Allocates; call it for hits only. *)
val explain : Scratch.t -> Weights.t -> Prepared.t -> Prepared.t -> Name_score.t
