(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit

let chop_suffix ~suffix s =
  if not (String.ends_with ~suffix s) then None else
  Some (String.sub s 0 (String.length s - String.length suffix))

let t_suffix = "-t.xml"
let target_suffix = "-target.xml"
let is_t_filename = chop_suffix ~suffix:t_suffix
let is_target_filename = chop_suffix ~suffix:target_suffix

type t =
  { id : string;
    ref : Trackmate.t; (* At least one exists, this is it for the metadata. *)
    t : Trackmate.t option;
    target : Trackmate.t option; }

let check mismatch g0 g1 = if g0 <> g1 then Fmt.str mismatch g0 g1 else ""

let v ~id ~t:t ~target:tgt = match t, tgt with
| None, None -> Error "No cell group provided"
| Some t, None -> Ok { id; ref = t; t = Some t; target = None }
| None, Some target -> Ok { id; ref = target; t = None; target = Some target }
| Some (t : Trackmate.t), Some (target : Trackmate.t) ->
    let e =
      String.concat ""
        [ check
            "Physical unit mismatch: %s for target cells and %s for T cells\n"
            t.physical_unit target.physical_unit;
          check
            "Physical unit %s but expected %s"
            t.physical_unit "micron";
          check
            "Time unit mismatch: %s for target cells and %s for T cells\n"
            t.time_unit target.time_unit;
          check
            "Time unit %s but expected %s"
            t.time_unit "sec";
          check
            "Frame number mismatch: %d for target cells and %d for T cells\n"
            t.nframes target.nframes
        ]
    in
    if e <> "" then Error e else
    Ok { id; ref = t; t = Some t; target = Some target }

let id o = o.id
let ref o = o.ref
let t o = o.t
let target o = o.target
let frame_count o = o.ref.Trackmate.nframes
let time_unit o = match o.ref.Trackmate.time_unit with "sec" -> "s" | u -> u
let time_interval o = o.ref.Trackmate.time_interval
let dur_of_frame_count o ~count =
  let unit = if time_unit o = "s" then "s" else "???" in
  ((float count) *. time_interval o), unit

let physical_unit o = match o.ref.Trackmate.physical_unit with
| "micron" -> "μm" | u -> u

let physical_size o =
  V2.mul o.ref.Trackmate.pixel_size o.ref.Trackmate.image_size

(* Per frame data *)

type 'a frames = 'a array
