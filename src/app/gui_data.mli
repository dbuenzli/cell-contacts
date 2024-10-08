(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Loading observations and results parameters. *)

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
  work_counter:Results_worker.Counter.t ->
  Observation.t option Note.signal * El.t

(** {1:groups Groups} *)

val cell_group :
  Results_worker.Counter.t ->
  scale:float option Note.signal ->
  min_max_distance:float option Note.signal ->
  ('a -> Trackmate.t option) ->
  'a option Note.signal -> Cell.Group.t option Note.signal

val intersect :
  Results_worker.Counter.t ->
  Cell.t Cell.Group.data option Note.signal ->
  Cell.t Cell.Group.data option Note.signal ->
  Cell.Group.intersections option Note.signal

(** {1:contacts Contacts} *)

val contacts :
  Results_worker.Counter.t ->
  Cell.Contact.spec Note.signal ->
  'a option Note.signal ->
  Cell.Contact.t list Cell.Group.data option Note.signal

val contact_stats :
  Cell.Contact.t list Cell.Group.data option Note.signal -> El.t

val download_csv :
  obs:Observation.t ->
  t:Cell.Group.t ->
  contacts:Cell.Contact.t list Cell.Group.data option ->
  Brr.El.t

val download_distances_csv :
  obs:Observation.t ->
  t:Cell.Group.t ->
  contacts:Cell.Contact.t list Cell.Group.data ->
  Brr.El.t
