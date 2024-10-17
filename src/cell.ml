(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit

let dump_ring =
  let dump_pt ppf pt = Fmt.pf ppf "%h, %h" (V2.x pt) (V2.y pt) in
  let iter_pts f c = Ring2.fold_pts (fun pt () -> f pt; ()) c () in
  Fmt.brackets @@ Fmt.iter ~sep:Fmt.semi iter_pts dump_pt

let dump_pgon =
  let iter_contours f p = Pgon2.fold_rings (fun c () -> f c; ()) p () in
  Fmt.brackets @@ Fmt.iter ~sep:Fmt.semi iter_contours dump_ring

type spot =
  { spot_id : Trackmate.spot_id;
    pos : P2.t;
    radius : float;
    area : float;
    pgon : Pgon2.t }

let nil_spot = { spot_id = - 1; pos = P2.o; radius = 0.; area = 0.;
                 pgon = Pgon2.empty }

let spot_of_tm_spot ?(scale = 1.) (s : Trackmate.spot) =
  (* N.B. trackmate contour data is given relative to s.pos *)
  let add center pt =
    if scale = 1. then V2.add center pt else V2.add center (V2.smul scale pt)
  in
  let c = List.map (add s.pos) s.contour in
  let c = Ring2.of_pts c in
  let pgon = Pgon2.of_rings [c] in
  let radius = scale *. s.radius in
  let area = scale *. scale *. s.area in
  { spot_id = s.sid; pos = s.pos; area; radius; pgon }

type id = Trackmate.track_id
type t =
  { track_id : id;
    frames : spot option Observation.frames; }

let[@inline] check_spot_frame frames s =
  let len = Array.length frames in
  if s.Trackmate.frame >= len
  then Fmt.failwith "Spot %d: frame %d not in [0;%d]" s.sid s.frame (len - 1)

let find_spot (tm : Trackmate.t) track_id id =
  match Trackmate.Int_map.find_opt id tm.spots_by_id with
  | None -> Fmt.failwith "Track %d: spot %d unknown" track_id id
  | Some s -> s

let frames_of_track ?scale (tm : Trackmate.t) (track : Trackmate.track) =
  let add_spot frames s =
    check_spot_frame frames s;
    match frames.(s.frame) with
    | None -> frames.(s.frame) <- Some (spot_of_tm_spot ?scale s)
    | Some s' when s'.spot_id = s.sid -> ()
    | Some s' ->
        Fmt.epr
          "%s: Track %d: frame %d: has more than one spot: %d and %d\n%!"
          tm.file track.tid s.frame s.sid s'.spot_id;
        ()
  in
  let add_edge tm frames (e : Trackmate.edge) =
    add_spot frames (find_spot tm track.tid e.spot_source_id);
    add_spot frames (find_spot tm track.tid e.spot_target_id);
  in
  let frames = Array.make tm.nframes None in
  List.iter (add_edge tm frames) track.edges;
  frames

let of_track ?scale tm t =
  { track_id = t.Trackmate.tid; frames = frames_of_track ?scale tm t }


module Group = struct
  type 'a data = 'a array
  type nonrec t = t data

  let of_trackmate ?scale ?(min_max_distance = -.max_float) (tm : Trackmate.t) =
    try
      let tracks =
        let not_dead tid =
          match Trackmate.Int_map.find_opt tid tm.tracks_by_id with
          | None -> Fmt.failwith "Unknown track: %d" tid
          | Some t ->
              if min_max_distance <= t.Trackmate.max_distance_traveled
              then Some (of_track ?scale tm t) else None
        in
        List.filter_map not_dead tm.filtered_tracks
      in
      Ok (Array.of_list tracks)
    with
    | Failure e -> Error e

  let frame_count g =
    if Array.length g = 0 then 0 else Array.length g.(0).frames

  type intersections = Pgon2.t option data data Observation.frames

  let err_frame_mismatch fc fc' =
    Fmt.error "Frame number mismatch in cell groups: %d and %d" fc fc'

  let log_err f c0 c1 err =
    let msg = match err with
    | `Edge_overlap -> "Edge overlap in input"
    | `Topology_panic msg ->
        (* Fmt.str "@[<v>%a@,and cell%d =@,%a@,and cell%d =@,%a@]"
           Fmt.lines msg c0 dump_pgon p0.pgon c1 dump_pgon p1.pgon *)
        msg
    in
    Fmt.epr "Warning: Frame %d: Cells %d %d: %a@." f c0 c1 Fmt.lines msg

  let intersections g0 g1 =
    let errs = Stdlib.ref 0 in
    let g0_frames = frame_count g0 in
    let g1_frames = frame_count g1 in
    if g0_frames <> g1_frames then err_frame_mismatch g0_frames g1_frames else
    let r _ = Array.make (Array.length g1) None in
    let r _ = Array.init (Array.length g0) r in
    let r = Array.init g0_frames r in
    for f = 0 to g0_frames - 1 do
      for c0 = 0 to Array.length g0 - 1 do
        for c1 = 0 to Array.length g1 - 1 do
          let p0 = g0.(c0).frames.(f) in
          let p1 = g1.(c1).frames.(f) in
          let isect = match p0, p1 with
          | None, _ | _, None -> None
          | Some p0, Some p1 ->
              match Pgon2.inter p0.pgon p1.pgon with
              | Error ((v, _), err) -> incr errs; log_err f c0 c1 err; Some v
              | Ok (v, _) -> Some v
          in
          r.(f).(c0).(c1) <- isect
        done
      done;
    done;
    Ok (r, !errs)

  let intersections_by_frames isect =
    let frame_isect isect i =
      let acc = Stdlib.ref [] in
      for g0 = 0 to Array.length isect.(i) - 1 do
        for g1 = 0 to Array.length isect.(i).(g0) - 1 do
          match isect.(i).(g0).(g1) with
          | None -> ()
          | Some p -> acc := p :: !acc
        done
      done;
      !acc
    in
    Array.init (Array.length isect) (frame_isect isect)


  let t_scale_default = 1.5
  let t_min_max_distance = 15.
end


module Imap = Map.Make (Int)

module Contact = struct
  type spec =
    { allowed_overlap_gap_length : int;
      min_overlap_pct : int; }

  let spec_default =
    { allowed_overlap_gap_length = 1;
      min_overlap_pct = 10; }

  type t =
    { target : Trackmate.track_id;
      start_frame : int;
      overlaps : float Observation.frames;
      distances : float Observation.frames;
      distance_max : int;
      dropped : int; }

  let frame_range c =
    (c.start_frame, c.start_frame + Array.length c.overlaps - 1)

  let sort_by_increasing_dur c0 c1 =
    -1 * (Int.compare (Array.length c0.overlaps) (Array.length c1.overlaps))

  let isect_area isect = (* FIXME Pgon2 *)
    let add c acc = Float.abs (Gg_kit.Ring2.area c) +. acc in
    (Gg_kit.Pgon2.fold_rings add isect 0.)


  let distances_to_start_frame ~normalize cell ~start_frame ~len =
    let a = Array.init len @@ fun i ->
      let s0 = cell.frames.(start_frame) in
      let s1 = cell.frames.(start_frame + i) in
      match s0, s1 with
      | None, _ | _, None -> nan
      | Some s0, Some s1 -> V2.norm V2.(s1.pos - s0.pos)
    in
    if not normalize then a else
    let max = Array.fold_left Float.max_num Float.min_float a in
    Array.map_inplace (fun v -> v /. max) a;
    a

  let find_max_distance_index distances =
    let max_d = ref Float.nan in
    let k = ref (-1) in
    for i = 0 to Array.length distances - 1 do
      let max = Float.max_num !max_d distances.(i) in
      if not (Float.equal max !max_d) then (max_d := max; k := i)
    done;
    !k

  let finish_contact spec contacts tcell target start_frame overlaps =
    let overlaps = Array.of_list (List.rev overlaps) in
    let distances =
      distances_to_start_frame ~normalize:false tcell
        ~start_frame ~len:(Array.length overlaps)
    in
    let distance_max = find_max_distance_index distances in
    contacts := { target; start_frame; overlaps; distances; distance_max;
                  dropped = 0 } ::
                !contacts

  let find spec ~t ~target ~isects =
    let cell_contacts t target isects i =
      let frame_count = Array.length isects in
      let cell = t.(i) in
      let active = Stdlib.ref Imap.empty in
      let try_stop target = function
      | None -> None
      | Some (allowed_gap, start_frame, overlaps) ->
          if allowed_gap = 0
          then None
          else Some (allowed_gap - 1, start_frame, overlaps)
      in
      let min_overlap_pct = float spec.min_overlap_pct /. 100. in
      for f = 0 to frame_count - 1 do match cell.frames.(f) with
      | None ->
          active := Imap.filter_map (fun k v -> try_stop k (Some v)) !active
      | Some spot ->
          let inv_cell_area = 1. /. spot.area in
          for target = 0 to Array.length target - 1 do
            match isects.(f).(i).(target) with
            | None ->
                active := Imap.update target (try_stop target) !active
            | Some isect ->
                let pct = isect_area isect *. inv_cell_area in
                if pct < min_overlap_pct then
                  active := Imap.update target (try_stop target) !active
                else begin
                  let update = function
                  | None -> Some (spec.allowed_overlap_gap_length, f, [pct])
                  | Some (allowed_gap, frame, os) ->
                      (* Add a 0% for the gaps (if any) and reset gap count. *)
                      let dd = spec.allowed_overlap_gap_length - allowed_gap in
                      let rec loop i os =
                        if i > 0 then loop (i - 1) (0.0 :: os) else os
                      in
                      let allowed_gap = spec.allowed_overlap_gap_length in
                      Some (allowed_gap, frame, pct :: loop dd os)
                  in
                  active := Imap.update target update !active
                end
          done;
      done;
      let contacts = Stdlib.ref [] in
      let finish target (_, start_frame, overlaps) =
        finish_contact spec contacts cell target start_frame overlaps
      in
      Imap.iter finish !active; active := Imap.empty;
      let cs = List.rev !contacts in
      match cs with
      | [] -> None
      | [c] -> Some c
      | cs ->
          let cs = List.sort sort_by_increasing_dur cs in
          let count = List.length cs in
          let dropped = count - 1 in
          let c = List.hd cs in
          Some ({c with dropped})
    in
    Array.init (Array.length t) (cell_contacts t target isects)

  type stats = { num_contacting : int; }

  let stats cs =
    let num_contacting = ref 0 in
    for i = 0 to Array.length cs - 1 do
      match cs.(i) with None -> () | Some c -> incr num_contacting;
    done;
    { num_contacting = !num_contacting }
end

let[@inline] spot_link_velocity dt s0 s1 =
  V2.norm (V2.(s1.pos - s0.pos)) /. dt

let frame_range_mean_speed_sum tm c ~first ~last =
  if first = last then 0., 0 else
  let frame_dt = tm.Trackmate.time_interval in
  let count = ref (-1) (* until we find the first spot *) in
  let velocity_sum = ref 0. in
  let dt = ref 0. in
  let last_spot = ref nil_spot in
  for i = first to last do match c.frames.(i) with
  | None -> if !count <> -1 then dt := !dt +. frame_dt
  | Some s ->
      if !count = -1 then (last_spot := s; count := 0) else begin
        dt := !dt +. frame_dt;
        let v = spot_link_velocity !dt !last_spot s in
        velocity_sum := !velocity_sum +. v;
        incr count;
        dt := 0.;
        last_spot := s;
      end
  done;
  !velocity_sum, (if !count < 0 then 0 else !count)

let frame_ranges_mean_speed tm c rs =
  (* This works on lists of intervals but nowadays we no longer use that. *)
  let rec loop tm c count sum = function
  | [] -> if count = 0 then 0. else (sum /. (float count))
  | (first, last) :: rs ->
      let rsum, rcount = frame_range_mean_speed_sum tm c ~first ~last in
      loop tm c (count + rcount) (sum +. rsum) rs
  in
  loop tm c 0 0. rs

let mean_speed tm c =
  (* That's just to make sure we understood everything well. *)
  frame_ranges_mean_speed tm c [0, Array.length c.frames - 1]

let mean_speed_contact tm cell c =
  frame_ranges_mean_speed tm cell [(Contact.frame_range c)]

let mean_speed_no_contact tm cell c =
  let ranges =
    if c.Contact.start_frame = 0 then [] else
    [(0, c.Contact.start_frame - 1)]
  in
  frame_ranges_mean_speed tm cell ranges
