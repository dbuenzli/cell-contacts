(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg

(** Trackmate model.

    [Parameter descriptions][pd]. Trackmate [repo][tm].

    [pd]: https://imagej.net/imagej-wiki-static/TrackMate_Algorithms.html
    [tm]: https://github.com/trackmate-sc/TrackMate *)

(** # Data model *)

module Int_set : Set.S with type elt = int
module Int_map : Map.S with type key = int

type spot_id = int
type spot =
  { sid : spot_id;
    frame : int;
    area : float;
    pos : P2.t;
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
    edges : edge list; }

type t =
  { file : string;
    physical_unit : string;
    time_unit : string;
    image_file : string;
    image_size : Size2.t;
    pixel_size : Size2.t;
    voxel_depth : float;
    nslices : int; (* ? *)
    nframes : int;
    time_interval : float;
    spots_by_id : spot Int_map.t;
    tracks_by_id  : track Int_map.t;
    filtered_tracks : track_id list; }

val pp_spot : Format.formatter -> spot -> unit
val pp_info : Format.formatter -> t -> unit

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
