(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg_kit
open Gg

(** Observation.

    Observations and derived computations. *)

val is_t_filename : string -> bool
val is_target_filename : string -> bool

(** {1:observations Observations} *)

type t
(** The type for observations. An observation gathers the
    trackmate data for T cells and target cells.  *)

val v :
  t:Trackmate.t option -> target:Trackmate.t option -> (t, string) result
(** [v ~t ~target] groups trackmate data for target cells and t
    cells. It checks that the metadata is compatible. At least one set
    must be provided. *)

val ref : t -> Trackmate.t
(** [ref o] is a reference trackmate data for the observation, can
    be used to lookup the metadata common metadata like time and physical
    units. *)

val t : t -> Trackmate.t option
(** [t o] are the T cells of [o] (if any). *)

val target : t -> Trackmate.t option
(** [target o] are the target cells of [o] (if any). *)

val frame_count : t -> int
(** [frame_count o] are the number of frames in the observation. *)

val time_unit : t -> string
(** [time_unit o] is the time unit of {!time_interval}. *)

val time_interval : t -> float
(** [time_interval_s o] is the time interval between two frames
    in {!time_interval} units. *)

val dur_of_frame_count : t -> count:int -> float * string
(** [dur_of_frame_count o ~count] is the duration of [count]
    frames and the unit as a string. *)

val physical_unit : t -> string
(** [physical_unit o] is the physical unit of {!physical_size}. *)

val physical_size : t -> V2.t
(** [physical_size o] is the physical size of the observation in
    {!physical_unit}s. *)

(** {1:per_frame Per frame data} *)

type 'a frames = 'a array
(** The type for data indexed by frame. *)
