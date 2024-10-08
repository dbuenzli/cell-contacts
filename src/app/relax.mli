(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Avoids long synchronous function calls (generic). *)

val run : (unit -> 'a) -> 'a Fut.t
val run' : (unit -> 'a Fut.t) -> 'a Fut.t

module E : sig
  val map : ('a -> 'b Fut.t) -> 'a Note.event -> 'b Note.event
end

module S : sig

  (** {b Warning.} For now using these functions leak. *)

  val map :
    ?eq:('b -> 'b -> bool) -> ('a -> 'b Fut.t) -> 'a Note.signal ->
    init:'b -> 'b Note.signal
  (** [map f s ~init] maps [s] with the asynchronous result of [f].
      The resulting signal always keeps its last value until a new result
      for [f] is available. No ordering on results is guaranted if your
      signal changes more often than results come in. *)

  val patience_map :
    ?eq:('b -> 'b -> bool) -> filler:('a -> 'b) -> ('a -> 'b Fut.t) ->
    'a Note.signal -> 'b Note.signal
  (** [patience_map ~filler f s] is like {!map} except whenever [s]
      changes [filler] is used as the signal value until the asynchronous
      results comes in. *)

end
