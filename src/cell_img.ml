(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Gg_kit
open Vg

let map_box ~src ~dst =
  M3.mul (M3.move2 (Box2.o dst)) @@
  M3.mul (M3.scale2 (V2.div (Box2.size dst) (Box2.size src))) @@
  M3.move2 (V2.neg (Box2.o src))

let fold_lefti f x a =
  let r = ref x in
  for i = 0 to Array.length a - 1 do
    r := f i !r (Array.unsafe_get a i)
  done;
  !r

let qual = Color_scheme.qualitative ~a:1.0 `Brewer_accent_8 ()

let t_color = qual 0
let target_color = qual 1
let isect_color = qual 5
let sel_color = qual 4

(* Geometry images *)

let cut_pt ~w pt color =
  let p = P.empty |> P.circle pt (0.5 *. w) in
  let outline = `O { P.o with width = 0.15 *. 0.5 *. w } in
  let full = Color.with_a color 1.0 in
  I.cut p (I.const color) |> I.blend (I.cut ~area:outline p (I.const full))

let ring_path ?(acc = P.empty) c =
  let add pt p = if p == acc then P.sub pt p else P.line pt p in
  P.close (Ring2.fold_pts add c acc)

let pgon_path p =
  let add c path = ring_path c ~acc:path in
  Pgon2.fold_rings add p P.empty

let cut_pgon ?(area = `Aeo) ?(o = I.const Color.black) ~w:width p i =
  let p = pgon_path p and outline = `O { P.o with width } in
  I.cut ~area p i |> I.blend (I.cut ~area:outline p o)

let cut_box ~w:width b color =
  let outline = `O { P.o with width } in
  I.cut ~area:outline (P.rect b P.empty) (I.const color)

let cut_seg ~w:width p0 p1 color =
  let area = `O { P.o with width } in
  I.cut ~area (P.empty |> P.sub p0 |> P.line p1) (I.const color)

let cell_track_path c =
  let add_pt p c = match c with
  | None -> p
  | Some c when P.is_empty p -> P.sub c.Cell.pos p
  | Some c -> P.line c.Cell.pos p
  in
  Array.fold_left add_pt P.empty c.Cell.frames

let cut_cell_track ~w:width c color =
  let area = `O { P.o with width } in
  I.cut ~area (cell_track_path c) (I.const color)

let group_cell_tracks g = I.void

let group_frame g col ~sel frame =
  let cut_cell ~w p col =
    let o = I.const (Color.with_a col 1.0) and a = I.const col in
    cut_pgon ~w:(2. *. w) ~o p a
  in
  let add_cell sel acc c = match c.Cell.frames.(frame) with
  | None -> acc
  | Some s ->
      let col = if c.track_id = sel then sel_color else col in
      I.blend (cut_pt ~w:1.5 s.pos Color.black) @@
      I.blend (cut_cell ~w:0.01 s.pgon col) @@
      acc
  in
  let sel = match sel with None -> -1 | Some i -> i in
  Array.fold_left (add_cell sel) I.void g

let isect_frame isect col i =
  let cut_isect ~w p col =
    let o = I.const (Color.with_a col 1.0) and a = I.const col in
    cut_pgon ~w:(2. *. w) ~o p a
  in
  let add_isect acc isect =
    acc |>
    I.blend (cut_isect ~w:0.01 isect col)
  in
  match isect with
  | None -> I.void
  | Some isect -> List.fold_left add_isect I.void isect.(i)

let group g =
  let frame_count = Cell.Group.frame_count g in
  List.init frame_count (group_frame g (qual 0) ~sel:None)

let groups_frame g0 g1 isect i =
  let g0 = group_frame g0 (qual 0) ~sel:None i in
  let g1 = group_frame g1 (qual 1) ~sel:None i in
  let g1_tracks = group_cell_tracks g1 in
  let isect = isect_frame isect (qual 5) i in
  g0 |> I.blend g1 |> I.blend g1_tracks |> I.blend isect

let groups g0 g1 isect =
  let frame_count = Cell.Group.frame_count g0 in
  let isect = Option.map Cell.Group.intersections_by_frames isect in
  List.init frame_count (groups_frame g0 g1 isect)

let tm_view ~aspect tm =
  let size = V2.mul (tm.Trackmate.image_size) (tm.Trackmate.pixel_size) in
  let flip img =
    I.scale (Size2.v 1.0 (-1.0)) @@ I.move (V2.v 0. (-.Size2.h size)) @@ img
  in
  Box2.v P2.o size, flip

let render_frame_page tm =
  let aspect = Size2.aspect tm.Trackmate.image_size in
  let page_size = Size2.of_w 100. (* mm *) ~aspect in
  let size = V2.mul (tm.Trackmate.image_size) (tm.Trackmate.pixel_size) in
  let view = Box2.v P2.o size in
  let flip image =
    I.scale (Size2.v 1.0 (-1.0)) @@ I.move (V2.v 0. (-.Size2.h size)) @@ image
  in
  fun image -> `Image (page_size, view, flip image)

let render_pdf
    ?(title = "T cell tracking") ?(description = "T cell tracking")
    ~dst tm frames
  =
  try
    Result.ok @@ Out_channel.with_open_bin dst @@ fun oc ->
    let xmp = Vgr.xmp ~title ~description () in
    let warn w = Vgr.pp_warning Format.err_formatter w in
    let font = None in
    let r = Vgr.create ~warn (Vgr_pdf.target ?font ~xmp ()) (`Channel oc) in
    let render_img = render_frame_page tm in
    List.iter (fun f -> ignore (Vgr.render r (render_img f))) frames;
    ignore (Vgr.render r `End);
    flush oc
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
