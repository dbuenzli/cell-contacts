(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg

module Xml = struct
  type t = Jv.t
  let by_tag_name d tag = Jv.call d "getElementsByTagName" Jv.[|of_string tag|]
  let length c = Jv.to_int (Jv.get c "length")
  let item c i = Jv.to_option Fun.id (Jv.call c "item" Jv.[|of_int i|])
  let data c = Jv.to_jstr (Jv.get c "textContent")
  let children c = Jv.get c "children"
  let parse s : t =
    let dom_parser = Jv.get Jv.global "DOMParser" in
    let dom_parser = Jv.new' dom_parser [||] in
    Jv.call dom_parser "parseFromString" Jv.[|of_jstr s; of_string "text/xml"|]
end

let att conv n node =
  match Jv.to_option Fun.id (Jv.call node "getAttribute" Jv.[|of_jstr n|]) with
  | None -> Fmt.failwith "missing attribute: %s" (Jstr.to_string n)
  | Some t -> conv (Jv.to_jstr t)

let atts n node = att Jstr.to_string n node
let attf n node = att Jstr.to_float n node
let atti n node =
  let to_int s = match Jstr.to_int s with
  | None -> failwith (Jstr.to_string s ^ ": not an int") | Some i -> i
  in
  att to_int n node

let filtered_tracks d =
  match Xml.item (Xml.by_tag_name d "FilteredTracks") 0 with
  | None -> []
  | Some t ->
      let c = Xml.children t in
      let acc = ref [] in
      for i = 0 to Xml.length c - 1 do
        match Xml.item c i with
        | None -> () | Some i ->
            acc := atti (Jstr.v "TRACK_ID") i :: !acc
      done;
      List.rev !acc

let spot_contour points =
  let rec loop acc = function
  | [] -> List.rev acc
  | v :: l when Jstr.is_empty v -> loop acc l
  | x :: y :: l ->
      loop ((P2.v (Jstr.to_float x) (Jstr.to_float y)) :: acc) l
  | _ -> failwith "Spot contour: even number of coordinates"
  in
  loop [] (Jstr.cuts ~sep:(Jstr.v " ") points)

let spots d =
  let spots = Xml.by_tag_name d "Spot" in
  let acc = ref Trackmate.Int_map.empty in
  for i = 0 to Xml.length spots - 1 do
    try match Xml.item spots i with
    | None -> ()
    | Some s ->
        let s =
          let sid = atti (Jstr.v "ID") s in
          let frame = atti (Jstr.v "FRAME") s in
          let area = attf (Jstr.v "AREA") s in
          let posx = attf (Jstr.v "POSITION_X") s in
          let posy = attf (Jstr.v "POSITION_Y") s in
          let radius = attf (Jstr.v "RADIUS") s in
          let pos = P2.v posx posy in
          let contour = spot_contour (Xml.data s) in
          { Trackmate.sid; frame; area; pos; radius; contour }
        in
        acc := Trackmate.Int_map.add s.Trackmate.sid s !acc;
    with
    | Failure e -> Fmt.failwith "Spot: %s" e
  done;
  !acc

let edges_of_children t =
  let cs = Xml.children t in
  let acc = ref [] in
  for i = 0 to Xml.length cs - 1 do match Xml.item cs i with
  | None -> ()
  | Some e ->
      let e =
        try
          let spot_source_id = atti (Jstr.v "SPOT_SOURCE_ID") e in
          let spot_target_id = atti (Jstr.v "SPOT_TARGET_ID") e in
          { Trackmate.spot_source_id; spot_target_id }
        with
        | Failure e -> Fmt.failwith "Edge tag parse error: %s, ignoring" e
      in
      acc := e :: !acc;
  done;
  !acc

let tracks d =
  let tracks = Xml.by_tag_name d "Track" in
  let acc = ref Trackmate.Int_map.empty in
  for i = 0 to Xml.length tracks - 1 do
    try match Xml.item tracks i with
    | None -> ()
    | Some t ->
        let t =
          let edges = edges_of_children t in
          let tid = atti (Jstr.v "TRACK_ID") t in
          let number_spots = atti (Jstr.v "NUMBER_SPOTS") t in
          let number_gaps = atti (Jstr.v "NUMBER_GAPS") t in
          let number_splits = atti (Jstr.v "NUMBER_SPLITS") t in
          let number_merges = atti (Jstr.v "NUMBER_MERGES") t in
          let number_complex = atti (Jstr.v "NUMBER_COMPLEX") t in
          let longest_gap = atti (Jstr.v "LONGEST_GAP") t in
          let track_duration = attf (Jstr.v "TRACK_DURATION") t in
          let track_start = attf (Jstr.v "TRACK_START") t in
          let track_stop = attf (Jstr.v "TRACK_STOP") t in
          let track_displacement = attf (Jstr.v "TRACK_DISPLACEMENT") t in
          let track_x_location = attf (Jstr.v "TRACK_X_LOCATION") t in
          let track_y_location = attf (Jstr.v "TRACK_Y_LOCATION") t in
          let track_z_location = attf (Jstr.v "TRACK_Z_LOCATION") t in
          let track_mean_speed = attf (Jstr.v "TRACK_MEAN_SPEED") t in
          let track_max_speed = attf (Jstr.v "TRACK_MAX_SPEED") t in
          let track_min_speed = attf (Jstr.v "TRACK_MIN_SPEED") t in
          let track_median_speed = attf (Jstr.v "TRACK_MEDIAN_SPEED") t in
          let track_std_speed = attf (Jstr.v "TRACK_STD_SPEED") t in
          let track_mean_quality = attf (Jstr.v "TRACK_MEAN_QUALITY") t in
          let total_distance_traveled =
            attf (Jstr.v "TOTAL_DISTANCE_TRAVELED") t in
          let max_distance_traveled =
            attf (Jstr.v "MAX_DISTANCE_TRAVELED") t in
          let confinement_ratio = attf (Jstr.v "CONFINEMENT_RATIO") t in
          let mean_straight_line_speed =
            attf (Jstr.v "MEAN_STRAIGHT_LINE_SPEED") t
          in
          let linearity_of_forward_progression =
            attf (Jstr.v "LINEARITY_OF_FORWARD_PROGRESSION") t
          in
          let mean_directional_change_rate =
            attf (Jstr.v "MEAN_DIRECTIONAL_CHANGE_RATE") t
          in
          { Trackmate.tid; number_spots; number_gaps; number_splits;
            number_merges;
            number_complex; longest_gap; track_duration; track_start;
            track_stop; track_displacement; track_x_location;
            track_y_location; track_z_location; track_mean_speed;
            track_max_speed; track_min_speed; track_median_speed;
            track_std_speed; track_mean_quality; total_distance_traveled;
            max_distance_traveled; confinement_ratio;
            mean_straight_line_speed; linearity_of_forward_progression;
            mean_directional_change_rate; edges }
        in
        acc := Trackmate.Int_map.add t.Trackmate.tid t !acc;
    with
    | Failure e -> Fmt.failwith "Track tag: %s" e
  done;
  !acc

let trackmate d ~file ~spots_by_id ~tracks_by_id ~filtered_tracks =
  match Xml.item (Xml.by_tag_name d "Model") 0 with
  | None -> failwith "Model tag: not found"
  | Some m ->
      let physical_unit = atts (Jstr.v "spatialunits") m in
      let time_unit = atts (Jstr.v "timeunits") m in
      match Xml.item (Xml.by_tag_name d "ImageData") 0 with
      | None -> failwith "ImageData tag: not found"
      | Some i ->
          try
            let fname = atts (Jstr.v "filename") i in
            let dir = atts (Jstr.v "folder") i in
            let w = attf (Jstr.v "width") i in
            let h = attf (Jstr.v "height") i in
            let nslices = atti (Jstr.v "nslices") i in
            let nframes = atti (Jstr.v "nframes") i in
            let pw = attf (Jstr.v "pixelwidth") i in
            let ph = attf (Jstr.v "pixelheight") i in
            let voxel_depth = attf (Jstr.v "voxeldepth") i in
            let time_interval = attf (Jstr.v "timeinterval") i in
            let image_file = Filename.concat dir fname in
            let image_size = Size2.v w h in
            let pixel_size = Size2.v pw ph in
            { Trackmate.file; physical_unit; time_unit; image_file;
              image_size; pixel_size; voxel_depth; nslices; nframes;
              time_interval;
              spots_by_id; tracks_by_id; filtered_tracks }
          with
          | Failure e -> Fmt.failwith "ImageData tag: %s" e

let of_jstr ?(file = Jstr.v "-") s =
  try
    let label = Jstr.(v "Parsing " + file) in
    Brr.Console.time label;
    let d = Xml.parse s in
    let file = Jstr.to_string file in
    let spots_by_id = spots d in
    let tracks_by_id = tracks d in
    let filtered_tracks = filtered_tracks d in
    let t = trackmate d ~file ~spots_by_id ~tracks_by_id ~filtered_tracks in
    Brr.Console.time_end label;
    Ok t
  with
  | Failure e -> Error (Jv.Error.v Jstr.( file + of_string ": " + of_string e))

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
