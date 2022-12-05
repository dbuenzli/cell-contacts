(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Brr

(* FIXME mess ! *)

let headers =
 [ "Id";
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
  let int i = Buffer.add_string b (string_of_int i); Buffer.add_char b ',' in
  let int_option = function None -> Buffer.add_char b ',' | Some i -> int i in
  let float f =
    Buffer.add_string b (string_of_float f); Buffer.add_char b ','
  in
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

let td_int_option i =
  let i = match i with None -> [] | Some i -> [El.txt (Jstr.of_int i)] in
  El.td ~at:At.[Negsp.Text.align `End] i

let td_int i =
  El.td ~at:At.[Negsp.Text.align `End] [El.txt (Jstr.of_int i)]

let td_float ?(frac = 2) f =
  El.td ~at:At.[Negsp.Text.align `End] [El.txt (Jstr.of_float ~frac f)]

let selected = Jstr.v "selected"

let tr sel_el sel_i on_click tm g contacts i =
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
   Copyright (c) 2022 The vz programmers

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
