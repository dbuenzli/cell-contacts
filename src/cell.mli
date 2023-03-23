(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
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

type t =
  { track_id : Trackmate.track_id; (** Track used to derive the cell. *)
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
end

(** {1:contact Contacts} *)

module Contact : sig
  type spec =
    { min_frame_count : int;
      min_overlap_pct : int; }

  type t =
    { target : Trackmate.track_id;
      start_frame : int;
      overlaps : float Observation.frames;
      kind : [ `Stable | `Transient ] }

  val find :
    spec -> t:Group.t -> target:Group.t ->
    isects:Group.intersections -> t list Group.data

  val count_stable_transient : t list -> int * int

  val unique_stable_count : t list -> int

  type stats =
    { num_contacting : int;
      max_target_contacts : int; }

  val stats : t list Group.data -> stats
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
