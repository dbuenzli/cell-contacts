(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

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

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
