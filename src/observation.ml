(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit

let is_t_filename = String.starts_with ~prefix:"t-"
let is_target_filename = String.starts_with ~prefix:"target-"

type t =
  { ref : Trackmate.t; (* At least one exists, this is it for the metadata. *)
    t : Trackmate.t option;
    target : Trackmate.t option; }

let check mismatch g0 g1 = if g0 <> g1 then Fmt.str mismatch g0 g1 else ""

let v ~t:t ~target:tgt = match t, tgt with
| None, None -> Error "No cell group provided"
| Some t, None -> Ok { ref = t; t = Some t; target = None }
| None, Some target -> Ok { ref = target; t = None; target = Some target }
| Some (t : Trackmate.t), Some (target : Trackmate.t) ->
    let e =
      String.concat ""
        [ check
            "Physical unit mismatch: %s for target cells and %s for T cells\n"
            t.physical_unit target.physical_unit;
          check
            "Time unit mismatch: %s for target cells and %s for T cells\n"
            t.time_unit target.time_unit;
          check
            "Frame number mismatch: %d for target cells and %d for T cells\n"
            t.nframes target.nframes
        ]
    in
    if e <> "" then Error e else
    Ok { ref = t; t = Some t; target = Some target }

let ref o = o.ref
let t o = o.t
let target o = o.target
let frame_count o = o.ref.Trackmate.nframes
let time_unit o = match o.ref.Trackmate.time_unit with "sec" -> "s" | u -> u
let time_interval o = o.ref.Trackmate.time_interval
let dur_of_frame_count o ~count =
  let unit = if time_unit o = "s" then "min" else "???" in
  ((float count) *. time_interval o) /. 60., unit

let physical_unit o = match o.ref.Trackmate.physical_unit with
| "micron" -> "μm" | u -> u

let physical_size o =
  V2.mul o.ref.Trackmate.pixel_size o.ref.Trackmate.image_size

(* Per frame data *)

type 'a frames = 'a array

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
