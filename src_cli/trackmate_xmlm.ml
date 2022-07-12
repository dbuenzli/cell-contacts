(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg

(* Xmlm helpers *)

module Smap = Map.Make (String)

type tree = E of Xmlm.tag * tree list | D of string

let att_map atts =
  let add acc ((_, k), v) = Smap.add k v acc in
  List.fold_left add Smap.empty atts

let get_att conv n atts = match Smap.find_opt n atts with
| None -> Fmt.failwith "missing attribute: %s" n | Some t -> conv t

(* XML parsing *)

let spot_contour_of_children = function
| [D points] ->
    let rec loop acc = function
    | [] -> List.rev acc
    | x :: y :: l ->
        loop ((P2.v (float_of_string x) (float_of_string y)) :: acc) l
    | _ -> failwith "Spot contour: even number of coordinates"
    in
    loop [] (String.split_on_char ' ' points)
| _ -> failwith "Spot: could not parse contour"

let spot_of_el (_, atts) cs =
  try
    let m = att_map atts in
    let sid = get_att int_of_string "ID" m in
    let frame = get_att int_of_string "FRAME" m in
    let area = get_att float_of_string "AREA" m in
    let posx = get_att float_of_string "POSITION_X" m in
    let posy = get_att float_of_string "POSITION_Y" m in
    let radius = get_att float_of_string "RADIUS" m in
    let pos = P2.v posx posy in
    let contour = spot_contour_of_children cs in
    { Trackmate.sid; frame; area; pos; radius; contour }
  with
  | Failure e -> Fmt.failwith "Spot: %s" e

let edge_of_tag (_, atts) =
  try
    let m = att_map atts in
    let spot_source_id = get_att int_of_string "SPOT_SOURCE_ID" m in
    let spot_target_id = get_att int_of_string "SPOT_TARGET_ID" m in
    { Trackmate.spot_source_id; spot_target_id }
  with
  | Failure e -> Fmt.failwith "Edge tag parse error: %s, ignoring" e

let edges_of_children cs =
  let rec loop acc = function
  | E (((_, "Edge"), _ as tag), _) :: cs -> loop ((edge_of_tag tag) :: acc) cs
  | _ :: cs -> loop acc cs
  | [] -> List.rev acc
  in
  loop [] cs

let track_of_el (_, atts) cs =
  try
    let edges = edges_of_children cs in
    let m = att_map atts in
    let tid = get_att int_of_string "TRACK_ID" m in
    let number_spots = get_att int_of_string "NUMBER_SPOTS" m in
    let number_gaps = get_att int_of_string "NUMBER_GAPS" m in
    let number_splits = get_att int_of_string "NUMBER_SPLITS" m in
    let number_merges = get_att int_of_string "NUMBER_MERGES" m in
    let number_complex = get_att int_of_string "NUMBER_COMPLEX" m in
    let longest_gap = get_att int_of_string "LONGEST_GAP" m in
    let track_duration = get_att float_of_string "TRACK_DURATION" m in
    let track_start = get_att float_of_string "TRACK_START" m in
    let track_stop = get_att float_of_string "TRACK_STOP" m in
    let track_displacement = get_att float_of_string "TRACK_DISPLACEMENT" m in
    let track_x_location = get_att float_of_string "TRACK_X_LOCATION" m in
    let track_y_location = get_att float_of_string "TRACK_Y_LOCATION" m in
    let track_z_location = get_att float_of_string "TRACK_Z_LOCATION" m in
    let track_mean_speed = get_att float_of_string "TRACK_MEAN_SPEED" m in
    let track_max_speed = get_att float_of_string "TRACK_MAX_SPEED" m in
    let track_min_speed = get_att float_of_string "TRACK_MIN_SPEED" m in
    let track_median_speed = get_att float_of_string "TRACK_MEDIAN_SPEED" m in
    let track_std_speed = get_att float_of_string "TRACK_STD_SPEED" m in
    let track_mean_quality = get_att float_of_string "TRACK_MEAN_QUALITY" m in
    let total_distance_traveled =
      get_att float_of_string "TOTAL_DISTANCE_TRAVELED" m in
    let max_distance_traveled =
      get_att float_of_string "MAX_DISTANCE_TRAVELED" m in
    let confinement_ratio = get_att float_of_string "CONFINEMENT_RATIO" m in
    let mean_straight_line_speed =
      get_att float_of_string "MEAN_STRAIGHT_LINE_SPEED" m
    in
    let linearity_of_forward_progression =
      get_att float_of_string "LINEARITY_OF_FORWARD_PROGRESSION" m
    in
    { Trackmate.tid; number_spots; number_gaps; number_splits; number_merges;
      number_complex; longest_gap; track_duration; track_start;
      track_stop; track_displacement; track_x_location;
      track_y_location; track_z_location; track_mean_speed;
      track_max_speed; track_min_speed; track_median_speed;
      track_std_speed; track_mean_quality; total_distance_traveled;
      max_distance_traveled; confinement_ratio;
      mean_straight_line_speed; linearity_of_forward_progression; edges }
  with
  | Failure e -> Fmt.failwith "Track tag: %s" e

let units_of_atts =
  let rec loop spatial time = function
  | ((_, "spatialunits"), v) :: l -> loop v time l
  | ((_, "timeunits"), v) :: l -> loop spatial v l
  | _ :: l -> loop spatial time l
  | [] -> spatial, time
  in
  loop "" ""

let image_data_of_atts atts =
  try
    let m = att_map atts in
    let fname = get_att Fun.id "filename" m in
    let dir = get_att Fun.id "folder" m in
    let w = get_att float_of_string "width" m in
    let h = get_att float_of_string "height" m in
    let nslices = get_att int_of_string "nslices" m in
    let nframes = get_att int_of_string "nframes" m in
    let pw = get_att float_of_string "pixelwidth" m in
    let ph = get_att float_of_string "pixelheight" m in
    let voxel_depth = get_att float_of_string "voxeldepth" m in
    let time_interval = get_att float_of_string "timeinterval" m in
    let physical_unit = "" in
    let time_unit = "" in
    let image_file = Filename.concat dir fname in
    let image_size = Size2.v w h in
    let pixel_size = Size2.v pw ph in
    { Trackmate.file = "";
      physical_unit; time_unit; image_file; image_size; pixel_size;
      voxel_depth; nslices; nframes; time_interval;
      spots_by_id = Trackmate.Int_map.empty;
      tracks_by_id = Trackmate.Int_map.empty;
      filtered_tracks = [] }
  with
  | Failure e -> Fmt.failwith "ImageData tag: %s" e

let filtered_tracks_of cs =
  let parse_trackid = function
  | E ((_, atts), _) ->
      let rec loop = function
      | [] -> None | ((_, "TRACK_ID"), v) :: _ -> Some (int_of_string v)
      | _ :: atts -> loop atts
      in
      loop atts
  | _ -> None
  in
  let rec loop acc = function
  | [] -> List.rev acc
  | e :: es ->
      match parse_trackid e with
      | None -> loop acc es
      | Some id -> loop (id :: acc) es
  in
  loop [] cs

let of_string ?(file = "-") src =
  try
    let units = ref ("", "") in
    let info = ref None in
    let spot_by_id = ref Trackmate.Int_map.empty in
    let track_by_id = ref Trackmate.Int_map.empty in
    let filtered_tracks = ref [] in
    let in_tree i =
      let el ((_, n), atts as tag) cs = match n with
      | "Spot" ->
          let s = spot_of_el tag cs in
          spot_by_id :=
            Trackmate.Int_map.add s.Trackmate.sid s !spot_by_id; D ""
      | "Track" ->
          let t = track_of_el tag cs in
          track_by_id :=
            Trackmate.Int_map.add t.Trackmate.tid t !track_by_id; D ""
      | "Model" -> units := units_of_atts atts; D ""
      | "ImageData" -> info := Some (image_data_of_atts atts); D ""
      | "FilteredTracks" -> filtered_tracks := filtered_tracks_of cs; D ""
      | _ -> E (tag, cs)
      in
      let data d = D d in
      Xmlm.input_doc_tree ~el ~data i
    in
    let i = Xmlm.make_input ~strip:true (`String (0, src)) in
    let _t = in_tree i in
    let info = match !info with
    | None -> failwith "No ImageData tag found"
    | Some info ->
        let physical_unit, time_unit = !units in
        { info with physical_unit; time_unit }
    in
    Ok { info with
         file; spots_by_id = !spot_by_id; tracks_by_id = !track_by_id;
         filtered_tracks = !filtered_tracks }
  with
  | Failure e -> Fmt.failwith "%s: %s" file e
  | Xmlm.Error ((l, c), e) ->
      Fmt.error "%s:%d:%d: %s" file l c (Xmlm.error_message e)

let of_file file =
  try
    let src = In_channel.with_open_bin file In_channel.input_all in
    of_string ~file src
  with
  | Sys_error e -> Error e

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
