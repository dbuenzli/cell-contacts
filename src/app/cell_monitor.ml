(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Note
open Note_brr
open Gg
open Vg

let bg = I.const (Color.gray 0.15)

let isect_img color time = function
| None -> I.void
| Some isect ->  isect color time

let group_img color time ~sel = function
| None -> I.void
| Some g -> Cell_img.group_frame g color ~sel time

let monitor time obs ~t ~target ~isect ~sel =
  let monitor time obs ((t, target), (isect, sel)) = lazy begin match obs with
  | None -> Box2.unit, bg
  | Some o ->
      let t = group_img Cell_img.t_color time ~sel t in
      let target = group_img Cell_img.target_color time ~sel:None target in
      let isect = Cell_img.isect_frame isect Cell_img.isect_color time in
      let view, flip = Cell_img.tm_view ~aspect:1.0 (Observation.ref o) in
      let img = bg |> I.blend target |> I.blend t |> I.blend isect in
      view, flip img
  end
  in
  let cells = S.Pair.v t target in
  let data =
    S.Pair.v (S.map (Option.map Cell.Group.intersections_by_frames) isect)
      sel
  in
  S.l3 ~eq:(==) monitor time obs (S.Pair.v cells data)

let max_frame = function None -> 0 | Some o -> Observation.frame_count o - 1

let time obs frame =
  let time_label o frame =
    let t, unit = match o with
    | None -> 0., "s"
    | Some o -> Observation.dur_of_frame_count o ~count:frame
    in
    let t = Printf.sprintf "%d" (int_of_float t) in
    let pad = 4 - Int.min (String.length t) 4 in
    let pad = String.concat "" (List.init pad (fun _ -> "\u{2007}")) in
    let t = pad ^ t in
    Printf.sprintf "t = %s%s" t unit
  in
  Output.span (S.l2 time_label obs frame)

let frame_selector obs =
  let min = S.const 0 and max = S.map max_frame obs in
  let label = `Txt "frame" in
  let enabled = S.Option.is_some obs in
  Input.int ~enabled ~text_size:3 ~label 0 ~min ~max

let frame_indicator obs =
  let frame, in_int = frame_selector obs in
  let at = Negsp.Layout.cluster ~gap:(`Sp `XXS) () in
  frame, El.div ~at [time obs frame; in_int]

let scale obs =
  let show = 100 (* XXX vz a percentage nice numbered would be better *) in
  let w = function
  | None -> Jstr.v "12%"
  | Some obs ->
      let w = Size2.w (Observation.physical_size obs) in
      let pct = truncate (((float 100) /. w) *. 100.) in
      Jstr.(of_int pct + v "%")
  in
  let scale = function
  | None -> Printf.sprintf "%dμm" show
  | Some obs -> Printf.sprintf "%d%s" show (Observation.physical_unit obs)
  in
  let at = At.[Negsp.Text.align `Center;
               class' (Jstr.v "scale");
(*               Negsp.Border.style `Solid;
               Negsp.Border.width Negsp.Size.thin;
               Negsp.Border.block_start Negsp.Size.zero;
               Negsp.Border.block_end Negsp.Size.zero *) ]
  in
  let scale = Output.block ~at (S.map scale obs) in
  let () = Elr.def_inline_style El.Style.width (S.map w obs) scale in
  scale

let color c =
  let r, g, b, a = Color.to_srgbi c in
  Printf.sprintf "rgba(%d,%d,%d,%f)" r g b a

let swatch c =
  let st =
    Jstr.v @@
    Printf.sprintf
      "background-color:%s;border-radius:50%%;\
       display:inline-block;width:1ch;height:1ch"
      (color c)
  in
  El.span ~at:At.[style st] []

let cell_count color kind g =
  let count = function
  | None -> "\u{2007}\u{2007}\u{2007}"
  | Some g -> Int.to_string (Array.length g)
  in
  El.div [ swatch color; El.sp (); El.txt' kind; El.sp ();
           Output.span (S.map count g); ]

let stats ~t ~target =
  let t = cell_count Cell_img.t_color "T" t in
  let target = cell_count Cell_img.target_color "Target" target in
  let isect =
    El.div [swatch Cell_img.isect_color; El.sp (); El.txt' "Overlap"]
  in
  let sel =
    El.div [swatch Cell_img.sel_color; El.sp (); El.txt' "Selected"]
  in
  let at =
    At.(style (Jstr.v "margin-top:calc(-1.0 * var(--sp_m))")) ::
    Negsp.(Text.size `S :: Layout.cluster ~gap:(`Sp `S) ())
  in
  El.div ~at [t; target; isect; sel]

let mouse_click e =
  let mouse_to_pt e ev = (* N.B. y = 0 is on top *)
    let ev = Ev.as_type ev in
    let x = (Ev.Mouse.client_x ev) -. El.bound_x e in
    let y = (Ev.Mouse.client_y ev) -. El.bound_y e in
    let nx = x /. (El.bound_w e) in
    let ny = y /. (El.bound_h e) in
    P2.v nx ny
  in
  Evr.on_el Ev.mouseup (mouse_to_pt e) e

let select (obs, frame, t) click = match Option.bind obs Observation.t with
| None -> None
| Some tm ->
    match t with
    | None -> None
    | Some t ->
        let click = V2.(mul tm.pixel_size (mul tm.image_size click)) in
        let rec loop max i =
          if i > max then None else
          match t.(i).Cell.frames.(frame) with
          | None -> loop max (i + 1)
          | Some spot ->
              let rsq = spot.radius *. spot.radius in
              if V2.norm2 (V2.(spot.pos - click)) <= rsq
              then Some t.(i).track_id else
              loop max (i + 1)
        in
        loop (Array.length t - 1) 0

let v obs ~t ~target ~isect ~sel =
  let frame, frame_el = frame_indicator obs in
  let monitor = monitor frame obs ~t ~target ~isect ~sel in
  let out = Output.image_view' monitor in
  let click_sel =
    let ss = S.l3 (fun x y z -> (x, y, z)) obs frame t in
    S.sample_filter ss ~on:(mouse_click out) select
  in
  let stats = stats ~t ~target in
  let box =
    let scale = scale obs in
    let at =
      Negsp.Text.size `XS ::
      At.style (Jstr.v "align-items:end") :: (* XXX Negsp *)
      Negsp.Layout.stack () ~gap:`Zero
    in
    El.div ~at [out; scale]
  in
  let at = Negsp.Layout.stack () ~gap:(`Sp `XXS) in
  let st = El.div ~at [box; stats; frame_el] in
  frame, click_sel, st
