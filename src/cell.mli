(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit

(** Cells.

    Use {!Group.of_trackmate} to get them. *)

(** {1:cell Cell data derivation} *)

type spot =
  { spot_id : Trackmate.spot_id; (** Spot used to derive the data. *)
    pos : P2.t;
    radius : float;
    area : float;
    pgon : Pgon2.t }

type id = Trackmate.track_id
(** The type for cell identifiers. That is the track id used to derive it. *)

type t =
  { track_id : id; (** Track used to derive the cell. *)
    frames : spot option Observation.frames;
    (** Spot of the cell in each frame (if any). *) }

(** Group of cells. *)
module Group : sig

  type 'a data = 'a array
  (** Per cell data. *)

  type nonrec t = t data
  (** A group of cells. *)

  val of_trackmate :
    ?scale:float -> ?min_max_distance:float -> Trackmate.t -> (t, string) result
  (** [of_trackmate tm] derives a cell group from the trackmate
      [tm]. Each filtered track found in the trackmate data defines a
      cell. [scale] scales the cell contour around their center
      (defaults to [1.0]). [min_max_distance] keeps only cells whose
      {!Trackmate.track.max_distance_traveled} is greater or equal than
      this value, defaults to [-. max_float] *)

  val frame_count : t -> int
  (** [frame_count g] is the number of frames for each cell in the group. *)

  type intersections = Pgon2.t option data data Observation.frames
  (** Group intersection matrix. See {!val-intersections}. *)

  val intersections : t -> t -> (intersections * int, string) result
  (** [intersection g0 g1] is [i] which has all the intersections
      between cells of [g0] and [g1]. The integer indicates if there
      were errors.

      The value at [i.(f).(id0).(id1)] is the intersection betwen cell
      [id1] of [g0] and [id0] of [g1] at frame [f] (if any). *)

  val intersections_by_frames : intersections -> Pgon2.t list Observation.frames
  (** [intersection_by_frames] has the intersections per frame.  *)

  (** {1:t_cell T cell defaults} *)

  val t_scale_default : float
  (** [t_scale_default] is the default scale value used for T cells. *)

  val t_min_max_distance : float
  (** [t_min_max_distance] is the default min_max_distance value used
      for T cells. *)
end

(** {1:contact Contacts} *)

module Contact : sig
  type spec =
    { allowed_overlap_gap_length : int;
      min_overlap_pct : int; }

  val spec_default : spec
  (** [spec_default] is the default contact specification. *)

  type t =
    { target : Trackmate.track_id; (** Intersecting target cell. *)
      start_frame : int; (** First touch. *)
      overlaps : float Observation.frames;
        (** Overlap values, starting at [start_frame] *)
      distances : float Observation.frames;
        (** Distance to first touch, starting at [start_frame] *)
      distance_max : int;
      (** Index of maximal value in [distance_max]. *)
      dropped : int;
      (** Number of alternative that were dropped. Only the longest
          one is kept. *)}

  val find :
    spec -> t:Group.t -> target:Group.t -> isects:Group.intersections ->
    t option Group.data
  (** Data grouped by t cell. *)

  type stats = { num_contacting : int; }

  val stats : t option Group.data -> stats
end

(** {1:speeds Computing speeds} *)

val mean_speed : Trackmate.t -> t -> float
(** [mean_speed tm c] is our own computation of the Mean sp. parameter
    computed by trackmate. It should yield the same result.
    [tm] is needed to get the time unit. *)

val mean_speed_contact : Trackmate.t -> t -> Contact.t -> float
(** [mean_speed_stable_contact tm c] computes the mean speed during the
    contact. *)

val mean_speed_no_contact : Trackmate.t -> t -> Contact.t -> float
(** [mean_speed_stable_contact tm c] computes the mean speed when there is
    no contact. *)
