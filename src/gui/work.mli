(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
