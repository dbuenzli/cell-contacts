(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg

module Counter : sig
  type t

  val make : unit -> t
  val incr : t -> unit
  val decr : t -> unit
  val value : t -> int Note.signal
  val is_zero : t -> bool Note.signal
end

type 'a t =
| Id : 'a -> 'a t
| Cell_group : Trackmate.t -> (Cell.Group.t, string) result t
| Cell_isect :
    Cell.Group.t * Cell.Group.t ->
    (Cell.Group.intersections * int, string) result t
| Cell_contacts :
    Cell.Contact.spec -> Cell.Contact.t list Cell.Group.data option t


val send : ?progress:(string option -> unit) -> 'a t -> 'a Fut.t
val setup : use_worker:bool -> unit
val run_worker : unit -> unit



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
