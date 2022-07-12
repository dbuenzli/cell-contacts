(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Note
open Brr
open Brr_note

open Gg
open Brr_canvas

let vz_class = Jstr.v "vz-output"

let set_content el = function
| `Txt "" -> ()
| `Txt txt -> El.append_children el [El.txt' txt]
| `Els els -> Elr.def_children el els

let make ?label outputs =
  (* FIXME we should make a caption. *)
  let cs = match label with
  | None -> outputs
  | Some `Txt "" -> outputs
  | Some content ->
      let label = El.label [] in
      set_content label content;
      label :: outputs
  in
  El.div ~at:[At.class' vz_class] cs

let image ?label ~size_mm ?(view = S.const (Box2.v P2.o size_mm)) i =
  let c = Brr_canvas.Canvas.create [] in
  let r = Vg.Vgr.create (Vgr_htmlc.target c) `Other in
  let render view i =
    ignore (Vg.Vgr.render r (`Image (size_mm, view, i)))
  in
  let log = Logr.(const render $ S.obs view $ S.obs i) in
  Logr.hold (Logr.create log);
  make ?label [Brr_canvas.Canvas.to_el c]

let resize_observer canvas resize =
  let ro = Jv.get Jv.global "ResizeObserver" in
  let ro = Jv.new' ro Jv.[| repr resize|] in
  ignore @@ Jv.call ro "observe" [|Brr_canvas.Canvas.to_jv canvas|]

let image_view ~view img =
  let st = Jstr.v "aspect-ratio: 1 / 1; width:100%; height: 100%;" in
  let c = Brr_canvas.Canvas.create ~at:At.[style st] [] in
  let r = Vg.Vgr.create (Vgr_htmlc.target ~resize:false c) `Other in
  let render view i =
    ignore @@ G.request_animation_frame @@ fun _ ->
    ignore (Vg.Vgr.render r (`Image (Gg.Size2.unit (* ignored *), view, i)))
  in
  let resize () =
    Brr_canvas.Canvas.set_size_to_layout_size c;
    render (S.value view) (S.value img)
  in
  let () = resize_observer c resize in
  let log = Logr.(const render $ S.obs view $ S.obs img) in
  Logr.hold (Logr.create log);
  Brr_canvas.Canvas.to_el c

let image_view' img =
  let st = Jstr.v "aspect-ratio: 1 / 1; width:100%; height: 100%;" in
  let c = Brr_canvas.Canvas.create ~at:At.[style st] [] in
  let r = Vg.Vgr.create (Vgr_htmlc.target ~resize:false c) `Other in
  let last = ref (Gg.Box2.unit, Vg.I.void) in
  let render i = ignore @@ G.request_animation_frame @@ fun _ ->
    let view, i as f = Lazy.force i in
    last := f;
    ignore (Vg.Vgr.render r (`Image (Gg.Size2.unit (* ignored *), view, i)))
  in
  let resize () =
    Brr_canvas.Canvas.set_size_to_layout_size c;
    render (Lazy.from_val !last)
  in
  let () = resize_observer c resize in
  let log = Logr.(const render $ S.obs img) in
  Logr.hold (Logr.create log);
  render (S.value img); (* XXX on FF sometime the first req is missed. *)
  Brr_canvas.Canvas.to_el c



let spinner spin =
  let el = Icon.arrow_path () in
  El.set_class (Jstr.v "spinner") true el;
  Elr.def_class (Jstr.v "spin") spin el;
  el

let span ?at txt =
  let sp = El.span ?at [] in
  let () = Elr.def_children sp (S.map (fun t -> [El.txt' t]) txt) in
  sp

let block ?(at = []) txt =
  let span = El.span ~at:At.(style (Jstr.v "display:inline-block") :: at)  [] in
  let () = Elr.def_children span (S.map (fun t -> [El.txt' t]) txt) in
  span

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
