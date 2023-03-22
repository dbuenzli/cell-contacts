(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr

module Settings : sig
  type t
  val t_scale : t -> float
  val t_min_max_distance : t -> float
  val contact_spec : t -> Cell.Contact.spec
  val input :
    obs:Observation.t option Note.signal ->
    enabled:bool Note.signal -> t Note.signal * El.t * El.t
end

(** {1:obs Observation data} *)

val input_obs :
  enabled:bool Note.signal ->
  work_counter:Work.Counter.t ->
  Observation.t option Note.signal * El.t

(** {1:groups Groups} *)

val cell_group :
  Work.Counter.t ->
  scale:float option Note.signal ->
  min_max_distance:float option Note.signal ->
  ('a -> Trackmate.t option) ->
  'a option Note.signal -> Cell.Group.t option Note.signal

val intersect :
  Work.Counter.t ->
  Cell.t Cell.Group.data option Note.signal ->
  Cell.t Cell.Group.data option Note.signal ->
  Cell.Group.intersections option Note.signal

(** {1:contacts Contacts} *)

val contacts :
  Work.Counter.t ->
  Cell.Contact.spec Note.signal ->
  'a option Note.signal ->
  Cell.Contact.t list Cell.Group.data option Note.signal


val contact_stats :
  Cell.Contact.t list Cell.Group.data option Note.signal -> El.t

val download_csv :
  tm:Trackmate.t ->
  t:Cell.Group.t ->
  contacts:Cell.Contact.t list Cell.Group.data option ->
  Brr.El.t

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
