(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr

(* Data encoders into table data and csv *)

type 'a enc = { td : 'a -> El.t; csv : Buffer.t -> 'a -> unit }

let int_enc =
  let td i = El.td ~at:At.[Negsp.Text.align `End] [El.txt (Jstr.of_int i)] in
  let csv b i =
    Buffer.add_string b (string_of_int i); Buffer.add_char b ','
  in
  { td; csv }

let int_opt_enc =
  let td i =
    let i = match i with None -> [] | Some i -> [El.txt (Jstr.of_int i)] in
    El.td ~at:At.[Negsp.Text.align `End] i
  in
  let csv b i =
    let i = match i with None -> "" | Some i -> Int.to_string i in
    Buffer.add_string b i; Buffer.add_char b ','
  in
  { td; csv }

let float_enc_with ~frac () =
  let td f =
    El.td ~at:At.[Negsp.Text.align `End] [El.txt (Jstr.of_float ~frac f)]
  in
  let csv b f =
    Buffer.add_string b (string_of_float f); Buffer.add_char b ','
  in
  { td; csv }

let float_enc = float_enc_with ~frac:2 ()

type 'a col =
  { name : string;
    href : string option;
    enc : 'a enc;
    get : Cell.t -> Trackmate.track -> 'a }

type ecol = C : 'a col -> ecol

let href h =
  Some ("https://imagej.net/imagej-wiki-static/TrackMate_Algorithms.html#" ^ h)

(*
  "Id";
  "Contacts";
  "Targets visited";
   *)

(* Track branching analyser *)

let number_spots =
  { name = "N spots"; href = href "Number_of_spots_in_track.";
    enc = int_enc; get = (fun _ t -> t.number_spots) }

let number_gaps =
  { name = "N gaps"; href = href "Number_of_gaps.";
    enc = int_enc; get = (fun _ t -> t.number_gaps); }

let longest_gap =
  { name = "Lgst gap"; href = href "Longest_gap.";
    enc = int_enc; get = (fun _ t -> t.longest_gap); }

let number_splits =
  { name = "N splits"; href = href "Number_of_split_events.";
    enc = int_enc; get = (fun _ t -> t.number_splits); }

let number_merges =
  { name = "N merges"; href = href "Number_of_merge_event.";
    enc = int_enc; get = (fun _ t -> t.number_merges); }

let number_complex =
  { name = "N complex"; href = href "Complex_points.";
    enc = int_enc; get = (fun _ t -> t.number_complex); }

(* Track duration *)

let track_duration =
  { name = "Duration"; href = href "Duration_of_track.";
    enc = float_enc; get = (fun _ t -> t.track_duration); }

let track_start =
  { name = "Start"; href = href "Track_start.";
    enc = float_enc; get = (fun _ t -> t.track_start); }

let track_stop =
  { name = "Stop"; href = href "Track_stop.";
    enc = float_enc; get = (fun _ t -> t.track_stop); }

let track_displacement =
  { name = "Displacement"; href = href "Track_displacement.";
    enc = float_enc; get = (fun _ t -> t.track_displacement) }

(* Track location *)

let track_x_location =
  { name = "X"; href = href "X_Location_.28mean.29.";
    enc = float_enc; get = (fun _ t -> t.track_x_location); }

let track_y_location =
  { name = "Y"; href = href "Y_Location_.28mean.29.";
    enc = float_enc; get = (fun _ t -> t.track_y_location); }

let track_z_location =
  { name = "Z"; href = href "Z_Location_.28mean.29.";
    enc = float_enc; get = (fun _ t -> t.track_z_location); }

(* Track speed *)

let track_mean_speed =
  { name = "Mean sp."; href = href "Mean_velocity.";
    enc = float_enc; get = (fun _ t -> t.track_mean_speed) }

let speed_derived =
  "Maximal_velocity.2C_Minimal_velocity.\
   2C_Median_velocity_and_Velocity_standard_deviation."

let track_max_speed =
  { name = "Max sp."; href = href speed_derived;
    enc = float_enc; get = (fun _ t -> t.track_max_speed) }

let track_min_speed =
  { name = "Min sp."; href = href speed_derived;
    enc = float_enc; get = (fun _ t -> t.track_min_speed) }

let track_median_speed =
  { name = "Med. sp."; href = href speed_derived;
    enc = float_enc; get = (fun _ t -> t.track_median_speed) }

let track_std_speed =
  { name = "Std sp."; href = href speed_derived;
    enc = float_enc; get = (fun _ t -> t.track_std_speed) }

(* Spot quality *)

let track_mean_quality =
  { name = "Mean Q"; href = href "Track_spot_quality.";
    enc = float_enc; get = (fun _ t -> t.track_mean_quality) }

(* Linear track analysis *)

let total_distance_traveled =
  { name = "Total dist."; href = href "Total_distance_traveled.";
    enc = float_enc; get = (fun _ t -> t.total_distance_traveled); }

let max_distance_traveled =
  { name = "Max dist."; href = href "Max_distance_traveled.";
    enc = float_enc; get = (fun _ t -> t.max_distance_traveled); }

let confinement_ratio =
  { name = "Cnf. ratio"; href = href "Confinement_ratio.";
    enc = float_enc; get = (fun _ t -> t.confinement_ratio); }

let mean_straigth_line_speed =
  { name = "Mean line sp."; href = href "Mean_straight_line_speed.";
    enc = float_enc; get = (fun _ t -> t.mean_straight_line_speed); }

let linearity_of_forward_progression =
  { name = "Fwd. progr.";
    href = href "Linearity_of_forward_progression.";
    enc = float_enc; get = (fun _ t -> t.linearity_of_forward_progression); }

let mean_directional_change_rate =
  { name = "Mn. y rate";
    href = href "Mean_directional_change.";
    enc = float_enc; get = (fun _ t -> t.mean_directional_change_rate); }

let cols =
  [ (* Track branching *)
    C number_spots; C number_gaps; C longest_gap;
    C number_splits; C number_merges; C number_complex;
    (* Track duration *)
    C track_duration; C track_start; C track_stop; C track_displacement;
    (* Track location *)
    C track_x_location; C track_y_location; C track_z_location;
    (* Track velocity *)
    C track_mean_speed; C track_max_speed; C track_min_speed;
    C track_median_speed; C track_std_speed;
    (* Spot quality *)
    C track_mean_quality;
    (* Linear track analysis *)
    C total_distance_traveled; C max_distance_traveled; C confinement_ratio;
    C mean_straigth_line_speed; C linearity_of_forward_progression;
  ]

(*
  "Mean speed";
  "Median speed";
  "Max speed";
  "Min speed";
  "Max dist.";
  "Total dist.";
  "Start time";
  "Stop time";
  "Splits";
  "Merges";
  "Gaps";
*)


let headers =
[
  "Id";
  "Contacts";
  "Targets visited";
  "Mean speed";
  "Median speed";
  "Max speed";
  "Min speed";
  "Max dist.";
  "Total dist.";
  "Start time";
  "Stop time";
  "Splits";
  "Merges";
  "Gaps"; ]


let to_csv tm targets contacts =
  let b = Buffer.create 5000 in
  let int = int_enc.csv b in
  let int_option = int_opt_enc.csv b in
  let float = float_enc.csv b in
  let add_header b h =
    Buffer.add_string b (Printf.sprintf {|"%s"|} h); Buffer.add_char b ','
  in
  List.iter (add_header b) headers;
  Buffer.add_char b '\n';
  for i = 0 to Array.length targets - 1 do
    let cell = targets.(i) in
    let unique =
      Option.map (fun c -> Cell.Contact.unique_count c.(i)) contacts
    in
    let contacts =
      Option.map (fun c -> Cell.Contact.count c.(i)) contacts
    in
    let t =
      Trackmate.Int_map.find_opt cell.Cell.track_id tm.Trackmate.tracks_by_id
    in
    let t = Option.get t in
    int t.Trackmate.tid;
    int_option contacts;
    int_option unique;
    float t.Trackmate.track_mean_speed;
    float t.Trackmate.track_median_speed;
    float t.Trackmate.track_max_speed;
    float t.Trackmate.track_min_speed;
    float t.Trackmate.max_distance_traveled;
    float t.Trackmate.total_distance_traveled;
    float t.Trackmate.track_start;
    float t.Trackmate.track_stop;
    int t.Trackmate.number_splits;
    int t.Trackmate.number_merges;
    int t.Trackmate.number_gaps;
    Buffer.add_char b '\n';
  done;
  Buffer.contents b

let thead g =
  El.thead ~at:At.[Negsp.Text.align `End] [
    El.tr [ El.th [El.txt' "Id"];
            El.th [El.txt' "Contacts"];
            El.th [El.txt' "Targets visited"];
            El.th [El.txt' "Median speed"];
            El.th [El.txt' "Max dist."];
            El.th [El.txt' "Total dist."];
            El.th [El.txt' "Start time"];
            El.th [El.txt' "Stop time"];
            El.th [El.txt' "Splits"];
            El.th [El.txt' "Merges"];
            El.th [El.txt' "Gaps"]]]

let selected = Jstr.v "selected"

let tr sel_el sel_i on_click tm g contacts i =
  let td_int = int_enc.td in
  let td_int_option = int_opt_enc.td in
  let td_float = float_enc.td in
  let cell = g.(i) in
  let unique = Option.map (fun c -> Cell.Contact.unique_count c.(i)) contacts in
  let contacts = Option.map (fun c -> Cell.Contact.count c.(i)) contacts in
  let t =
    Trackmate.Int_map.find_opt cell.Cell.track_id tm.Trackmate.tracks_by_id
  in
  let t = Option.get t in
  let tr = El.tr [
      td_int t.Trackmate.tid;
      td_int_option contacts;
      td_int_option unique;
      td_float t.Trackmate.track_median_speed;
      td_float t.Trackmate.max_distance_traveled;
      td_float t.Trackmate.total_distance_traveled;
      td_float (t.Trackmate.track_start /. 60.);
      td_float (t.Trackmate.track_stop /. 60.);
      td_int t.Trackmate.number_splits;
      td_int t.Trackmate.number_merges;
      td_int t.Trackmate.number_gaps]
  in
  ignore (Ev.listen Ev.click (on_click i) (El.as_target tr));
  begin match sel_i with
  | Some j when i = j -> El.set_class selected true tr; sel_el := Some tr
  | _ -> ()
  end;
  tr

let of_cell_group tm g ~contacts ~sel =
  let sel_i = match sel with
  | None -> None
  | Some i ->
      match contacts with
      | None -> None
      | Some c ->
          if i < 0 || i > Array.length c then None else Some i
  in
  let sel, set_sel = Note.S.create sel_i in
  let sel_el = ref None in
  let on_click i ev =
    let t = El.of_jv (Ev.target_to_jv (Ev.current_target ev)) in
    match !sel_el with
    | None ->
        El.set_class selected true t; sel_el := Some t; set_sel (Some i)
    | Some sel when sel == t -> ()
    | Some sel ->
        El.set_class selected false sel;
        El.set_class selected true t;
        sel_el := Some t; set_sel (Some i)
  in
  let thead = thead g in
  let tbody =
    El.tbody (List.init (Array.length g)
                (tr sel_el sel_i on_click tm g contacts))
  in
  let el =
    El.div
      ~at:At.[class' (Jstr.v "datatable"); Negsp.Text.size `S]
      [El.table [thead; tbody]]
  in
  sel, el

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
