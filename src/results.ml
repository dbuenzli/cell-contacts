(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let cell_id obs id =
    String.concat "-" [Observation.id obs; Printf.sprintf "%04d" id]

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

type 'a col =
  { name : string;
    name_th : string; (* If "" [name] is used *)
    href : string option;
    enc : 'a enc;
    get :
      Observation.t -> Trackmate.t ->
      Cell.t -> Trackmate.track -> Cell.Contact.info option -> 'a }

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
  { name = "Duration (s)"; name_th = "";
    href = href "Duration_of_track.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_duration); }

let track_start =
  { name = "Start (s)"; name_th = ""; href = href "Track_start.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_start); }

let track_stop =
  { name = "Stop (s)"; name_th = ""; href = href "Track_stop.";
    enc = float_enc; get = (fun _ _ _ t _ -> t.track_stop); }

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
    enc = string_enc; get = (fun obs _ _ t _ -> cell_id obs t.tid) }

(* Contact *)

let stable =
  { name = "Stable"; name_th = ""; href = None;
    enc = int_enc;
    get = fun _ _ _ _ c ->
      let count c = if Option.is_some c.Cell.Contact.stable then 1 else 0 in
      Option.value ~default:0 (Option.map count c) }

let stable_contact_start =
  { name = "St start (s)"; name_th = ""; href = None;
    enc = float_opt_enc;
    get = fun obs _ _ _ c -> match c with
    | None -> None
    | Some c ->
      let start_frame st =
        (float st.Cell.Contact.start_frame) *. Observation.time_interval obs
      in
      Option.map start_frame c.stable }

let stable_contact_len =
  { name = "St dur (s)"; name_th = ""; href = None;
    enc = float_opt_enc;
    get = fun obs _ _ _ c -> match c with
    | None -> None
    | Some c ->
        let dur st =
          (float (Array.length st.Cell.Contact.overlaps)) *.
          Observation.time_interval obs
        in
        Option.map dur c.stable; }

let stable_contact_max_dist =
  { name = "St max dist."; name_th = ""; href = None;
    enc = float_opt_enc;
    get = fun _ _ _ _ c -> match c with
    | None -> None
    | Some c ->
        let max c = c.Cell.Contact.distances.(c.Cell.Contact.distance_max) in
        Option.map max c.stable }

let stable_contact_dur_to_max_dist =
  { name = "St dur to max dist. (s)"; name_th = ""; href = None;
    enc = float_opt_enc;
    get = fun obs _ _ _ c -> match c with
    | None -> None
    | Some c ->
        let dur c =
          (float c.Cell.Contact.distance_max) *. Observation.time_interval obs
        in
        Option.map dur c.stable}

let our_track_mean_speed =
  { name = "Mean sp. (ctrl)"; name_th = ""; href = None;
    enc = float_enc;
    get = (fun _ tm c _ _ -> Cell.mean_speed c); }

let mean_speed_stable_contact =
  { name = "Mean sp. st"; name_th = ""; href = None;
    enc = float_opt_enc;
    get = (fun _ _ _ _ c -> match c with
      | None -> None
      | Some c ->
          match c.stable with
          | None -> None
          | Some c -> Some c.Cell.Contact.mean_speed) }

let mean_speed_no_contact =
  (* Note if there is no contact this should be equal to track_mean_speed *)
  { name = "Mean sp. no ctc"; name_th = ""; href = None;
    enc = float_opt_enc;
    get = (fun _ _ _ _ c -> match c with
      | None -> None
      | Some c -> Some c.Cell.Contact.mean_speed_no_contact) }

let cols =
  [ C id;
    C stable;
    C stable_contact_start;
    C stable_contact_len;
    C stable_contact_max_dist;
    C stable_contact_dur_to_max_dist;
    C mean_speed_stable_contact;
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
    C track_mean_quality; ]

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

let get_distances ~normalize contact =
  if not normalize then contact.Cell.Contact.distances else
  let max_d =
    contact.Cell.Contact.distances.(contact.Cell.Contact.distance_max)
  in
  Array.map (fun v -> v /. max_d) contact.Cell.Contact.distances

let contact_distances_to_csv ~normalize ~headers ~obs ~t:cells ~contacts =
  let add_float_array b a =
    let max = Array.length a - 1 in
    if max < 0 then () else
    begin
      Buffer.add_char b ',';
      for i = 0 to max do
        Buffer.add_string b (string_of_float a.(i));
        if i <> max then Buffer.add_char b ',';
      done
    end
  in
  let add_headers b =
    Buffer.add_string b "Id";
    Buffer.add_char b ',';
    Buffer.add_string b "Ctx start (fr.)";
    Buffer.add_string b "\r\n";
  in
  let b = Buffer.create 5000 in
  if headers then add_headers b;
  for i = 0 to Array.length cells - 1 do
    let cell = cells.(i) in
    match contacts.(i).Cell.Contact.stable with
    | None -> ()
    | Some contact ->
        let ds = get_distances ~normalize contact in
        Buffer.add_string b (cell_id obs cell.Cell.track_id);
        Buffer.add_char b ',';
        Buffer.add_string b (string_of_int contact.Cell.Contact.start_frame);
        add_float_array b ds;
        Buffer.add_string b "\r\n";
  done;
  Buffer.contents b

let contact_distances_to_json_objs ~normalize ~obs ~t:cells ~contacts =
  let acc = ref [] in
  let add_float_array b a =
    let max = Array.length a - 1 in
    begin
      Buffer.add_char b '[';
      for i = 0 to max do
        let v = Printf.sprintf "%.16f" a.(i) in
        let v = if v = "nan" then "null" else v in
        Buffer.add_string b v;
        if i <> max then Buffer.add_char b ',';
      done;
      Buffer.add_char b ']';
    end
  in
  let b = Buffer.create 5000 in
  let max = Array.length cells - 1 in
  for i = 0 to max do
    let cell = cells.(i) in
    match contacts.(i).Cell.Contact.stable with
    | None -> ()
    | Some contact ->
        let ds = get_distances ~normalize contact in
        Buffer.add_string b "{\"cell\":";
        Buffer.add_char b '\"';
        Buffer.add_string b (cell_id obs cell.Cell.track_id);
        Buffer.add_char b '\"';
        Buffer.add_char b ',';
        Buffer.add_string b "\"Ctx start (fr.)\":";
        Buffer.add_string b (string_of_int contact.Cell.Contact.start_frame);
        Buffer.add_char b ',';
        Buffer.add_string b "\"distances\":";
        add_float_array b ds;
        Buffer.add_char b '}';
        acc := (Buffer.contents b) :: !acc;
        Buffer.reset b
  done;
  List.rev !acc
