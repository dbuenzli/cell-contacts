(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg


val t_color : Color.t
val target_color : Color.t
val isect_color : Color.t
val sel_color : Color.t

val group_frame : Cell.Group.t -> Color.t -> sel:int option -> int -> Vg.image

val isect_frame :
  Gg_kit.Pgon2.t list array option -> Color.t -> int -> Vg.image

val group : Cell.Group.t -> Vg.image list
val groups :
  Cell.Group.t -> Cell.Group.t -> Cell.Group.intersections option ->
  Vg.image list

val tm_view : aspect:float -> Trackmate.t -> Box2.t * (Vg.image -> Vg.image)
val render_pdf :
  ?title:string -> ?description:string ->
  dst:string -> Trackmate.t -> Vg.image list -> (unit, string) result

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
