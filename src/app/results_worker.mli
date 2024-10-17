(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Compute cell results on a web worker. *)

open Gg

module Counter : sig
  type t

  val make : unit -> t
  val incr : t -> unit
  val decr : t -> unit
  val value : t -> int Note.signal
  val is_zero : t -> bool Note.signal
end

type 'a work =
| Id : 'a -> 'a work
| Cell_group : Trackmate.t -> (Cell.Group.t, string) result work
| Cell_isect :
    Cell.Group.t * Cell.Group.t ->
    (Cell.Group.intersections * int, string) result work
| Cell_contacts :
    Cell.Contact.spec -> Cell.Contact.t option Cell.Group.data option work

val send : ?progress:(string option -> unit) -> 'a work -> 'a Fut.t
val setup : use_worker:bool -> unit
val run : unit -> unit
