(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Data encoders into table data and csv *)

type 'a enc = { csv : Buffer.t -> 'a -> unit; td : 'a -> string }

let string_enc =
  let csv b s = Buffer.add_string b s in
  let td = Fun.id in
  { csv; td }

let int_enc =
  let csv b i = Buffer.add_string b (string_of_int i) in
  let td i = string_of_int i in
  { csv; td }

let int_opt_enc =
  let csv b = function
  | None -> () | Some i -> Buffer.add_string b (Int.to_string i)
  in
  let td = function None -> "" | Some i -> string_of_int i in
  { csv; td }

let float_enc_with ~frac () =
  let csv b f = Buffer.add_string b (string_of_float f) in
  let td f = Printf.sprintf "%.*f" frac f in
  { csv; td }

let float_enc = float_enc_with ~frac:2 ()

let float_opt_enc =
  let csv b = function
  | None -> () | Some f -> Buffer.add_string b (string_of_float f) in
  let td = function None -> "" | Some f -> Printf.sprintf "%.2f" f in
  { csv; td }

let time_enc = (* We show it with another unit in the ui *)
  let to_s = 1. /. 60. in
  let csv b f = Buffer.add_string b (string_of_float f) in
  let td f = Printf.sprintf "%.2f" (f *. to_s) in
  { csv; td }

type 'a col =
  { name : string;
    name_th : string; (* If "" [name] is used *)
    href : string option;
    enc : 'a enc;
    get :
      Observation.t -> Trackmate.t ->
      Cell.t -> Trackmate.track -> Cell.Contact.t list option -> 'a }

type ecol = C : 'a col -> ecol

let href h =
  Some ("https://imagej.net/imagej-wiki-static/TrackMate_Algorithms.html#" ^ h)

(* Track branching analyser *)

let number_spots =
  { name = "N spots"; name_th = ""; href = href "Number_of_spots_in_track.";
    enc = int_enc; get = (fun _ _ _ t _ -> t.number_spots) }

let number_gaps =
  { name = "N gaps"; name_th = ""; href = href "Number_of_gaps.";
    enc = int_enc; get = (fun _ _ _ t _ -> t.number_gaps); }

let longest_gap =
  { name = "Lgst gap"; name_th = ""; href = href "Longest_gap.";
    enc = int_enc; get = (fun _ _ _ t _ -> t.longest_gap); }

let number_splits =
  { name = "N splits"; name_th = ""; href = href "Number_of_split_events.";
    enc = int_enc; get = (fun _ _ _ t _ -> t.number_splits); }

let number_merges =
  { name = "N merges"; name_th = ""; href = href "Number_of_merge_event.";
    enc = int_enc; get = (fun _ _ _ t _  -> t.number_merges); }

let number_complex =
  { name = "N complex"; name_th = ""; href = href "Complex_points.";
    enc = int_enc; get = (fun _ _ _ t _ -> t.number_complex); }

(* Track duration *)

let track_duration =
  { name = "Duration"; name_th = "Duration (s)";
    href = href "Duration_of_track.";
    enc = time_enc; get = (fun _ _ _ t _ -> t.track_duration); }

let track_start =
  { name = "Start"; name_th = "Start (s)"; href = href "Track_start.";
    enc = time_enc; get = (fun _ _ _ t _ -> t.track_start); }

let track_stop =
  { name = "Stop"; name_th = "Stop (s)"; href = href "Track_stop.";
    enc = time_enc; get = (fun _ _ _ t _ -> t.track_stop); }

let track_displacement =
  { name = "Displacement"; name_th = ""; href = href "Track_displacement.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_displacement) }

(* Track location *)

let track_x_location =
  { name = "X"; name_th = ""; href = href "X_Location_.28mean.29.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_x_location); }

let track_y_location =
  { name = "Y"; name_th = ""; href = href "Y_Location_.28mean.29.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_y_location); }

let track_z_location =
  { name = "Z"; name_th = ""; href = href "Z_Location_.28mean.29.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_z_location); }

(* Track speed *)

let track_mean_speed =
  { name = "Mean sp."; name_th = ""; href = href "Mean_velocity.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_mean_speed) }

let speed_derived =
  "Maximal_velocity.2C_Minimal_velocity.\
   2C_Median_velocity_and_Velocity_standard_deviation."

let track_max_speed =
  { name = "Max sp."; name_th = ""; href = href speed_derived;
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_max_speed) }

let track_min_speed =
  { name = "Min sp."; name_th = ""; href = href speed_derived;
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_min_speed) }

let track_median_speed =
  { name = "Med. sp."; name_th = ""; href = href speed_derived;
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_median_speed) }

let track_std_speed =
  { name = "Std sp."; name_th = ""; href = href speed_derived;
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_std_speed) }

(* Spot quality *)

let track_mean_quality =
  { name = "Mean Q"; name_th = ""; href = href "Track_spot_quality.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_mean_quality) }

(* Linear track analysis *)

let total_distance_traveled =
  { name = "Total dist."; name_th = ""; href = href "Total_distance_traveled.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.total_distance_traveled); }

let max_distance_traveled =
  { name = "Max dist."; name_th = ""; href = href "Max_distance_traveled.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.max_distance_traveled); }

let confinement_ratio =
  { name = "Cnf. ratio"; name_th = ""; href = href "Confinement_ratio.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.confinement_ratio); }

let mean_straigth_line_speed =
  { name = "Mean line sp."; name_th = "";
    href = href "Mean_straight_line_speed.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.mean_straight_line_speed); }

let linearity_of_forward_progression =
  { name = "Fwd. progr.";
    name_th = ""; href = href "Linearity_of_forward_progression.";
    enc = float_enc;
    get = (fun _ _ _ t _ -> t.linearity_of_forward_progression); }

let mean_directional_change_rate =
  { name = "Mn. y rate";
    name_th = ""; href = href "Mean_directional_change.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.mean_directional_change_rate); }

let id =
  { name = "Id";
    name_th = ""; href = href "Track_ID.";
    enc = string_enc; get = (fun obs _ _ t _ ->
      Observation.id obs ^ Printf.sprintf "%03d" t.tid) }

let contacts =
  { name = "Contacts"; name_th = ""; href = None;
    enc = int_opt_enc;
    get = (fun _ _ _ _ c ->
        Option.map (fun c -> fst (Cell.Contact.count_stable_transient c)) c); }

let transient =
  { name = "Transient"; name_th = ""; href = None;
    enc = int_opt_enc;
    get = (fun _ _ _ _ c ->
        Option.map (fun c -> snd (Cell.Contact.count_stable_transient c)) c) }

let targets_visited =
  { name = "Tgt visited"; name_th = ""; href = None;
    enc = int_opt_enc;
    get = (fun _ _ _ _ c -> Option.map Cell.Contact.unique_stable_count c) }

let our_track_mean_speed =
  { name = "Mean sp. (ctrl)"; name_th = ""; href = None;
    enc = float_enc;
    get = (fun _ tm c _ _ -> Cell.mean_speed tm c); }

let mean_speed_stable_contact =
  { name = "Mean sp. stbl."; name_th = ""; href = None;
    enc = float_opt_enc;
    get = (fun _ tm cell _ cs ->
        Option.map (Cell.mean_speed_stable_contact tm cell) cs) }

let mean_speed_transient_contact =
  { name = "Mean sp. trnst."; name_th = ""; href = None;
    enc = float_opt_enc;
    get = (fun _ tm cell _ cs ->
        Option.map (Cell.mean_speed_transient_contact tm cell) cs) }

let mean_speed_no_contact =
  { name = "Mean sp. no ctc."; name_th = ""; href = None;
    enc = float_opt_enc;
    get = (fun _ tm cell _ cs ->
        Option.map (Cell.mean_speed_no_contact tm cell) cs) }

let cols =
  [ C id;
    C contacts;
    C transient;
    C targets_visited;
    C mean_speed_stable_contact;
    C mean_speed_transient_contact;
    C mean_speed_no_contact;
(*    C our_track_mean_speed; *)
    C track_mean_speed;
    C track_start; C track_stop;
    C max_distance_traveled;
    (* Linear track analysis *)
    C total_distance_traveled; C confinement_ratio;
    C mean_straigth_line_speed; C linearity_of_forward_progression;
    (* Track velocity *)
    C track_max_speed; C track_min_speed;
    C track_median_speed; C track_std_speed;
    (* Track duration *)
    C track_duration; C track_displacement;
    (* Track branching *)
    C number_spots; C number_gaps; C longest_gap;
    C number_splits; C number_merges; C number_complex;
    (* Track location *)
    C track_x_location; C track_y_location; C track_z_location;
    (* Spot quality *)
    C track_mean_quality;
 ]

let to_csv ~headers ~obs ~t:cells ~contacts =
  let tm = Observation.t obs |> Option.get in
  let b = Buffer.create 5000 in
  let rec add_headers b = function
  | [] -> Buffer.add_string b "\r\n"
  | (C c) :: cs ->
      Buffer.add_string b (Printf.sprintf {|"%s"|} c.name);
      if cs <> [] then Buffer.add_char b ',';
      add_headers b cs
  in
  let rec add_row b tm cell track contacts = function
  | [] -> Buffer.add_string b "\r\n"
  | (C c) :: cs ->
      let v = c.get obs tm cell track contacts in
      c.enc.csv b v; if cs <> [] then Buffer.add_char b ',';
      add_row b tm cell track contacts cs
  in
  if headers then add_headers b cols;
  for i = 0 to Array.length cells - 1 do
    let cell = cells.(i) in
    let track =
      Option.get @@
      Trackmate.Int_map.find_opt cell.Cell.track_id tm.Trackmate.tracks_by_id
    in
    let contacts = Option.map (fun c -> c.(i)) contacts in
    add_row b tm cell track contacts cols
  done;
  Buffer.contents b
