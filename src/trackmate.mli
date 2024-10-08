(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg

(** Trackmate model.

    {{:https://imagej.net/imagej-wiki-static/TrackMate_Algorithms.html}
    Parameter descriptions}.
    Trackmate {{:https://github.com/trackmate-sc/TrackMate}repo} *)

(** {1:model Data model} *)

module Int_set : Set.S with type elt = int
module Int_map : Map.S with type key = int

type spot_id = int
type spot =
  { sid : spot_id;
    frame : int;
    area : float;
    pos : P2.t; (** Origin is top left of the image, physical units. *)
    radius : float;
    contour : P2.t list }

type edge =
  { spot_source_id : spot_id;
    spot_target_id : spot_id; }

type track_id = int
type track =
  { tid : track_id;
    number_spots : int;
    number_gaps : int;
    number_splits : int;
    number_merges : int;
    number_complex : int;
    longest_gap : int;
    track_duration : float;
    track_start : float;
    track_stop : float;
    track_displacement : float;
    track_x_location : float;
    track_y_location : float;
    track_z_location : float;
    track_mean_speed : float;
    track_max_speed : float;
    track_min_speed : float;
    track_median_speed : float;
    track_std_speed : float;
    track_mean_quality : float;
    total_distance_traveled : float;
    max_distance_traveled : float;
    confinement_ratio : float;
    mean_straight_line_speed : float;
    linearity_of_forward_progression : float;
    mean_directional_change_rate : float;
    edges : edge list; }

type t =
  { file : string;
    physical_unit : string;
    time_unit : string;
    image_file : string;
    image_size : Size2.t; (** In pixels *)
    pixel_size : Size2.t; (** Size of pixels in physical units. *)
    voxel_depth : float;
    nslices : int; (* ? *)
    nframes : int;
    time_interval : float;
    spots_by_id : spot Int_map.t;
    tracks_by_id  : track Int_map.t;
    filtered_tracks : track_id list; }

val pp_spot : Format.formatter -> spot -> unit
val pp_info : Format.formatter -> t -> unit
