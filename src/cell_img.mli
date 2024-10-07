(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg


val t_color : Color.t
val target_color : Color.t
val isect_color : Color.t
val sel_color : Color.t

val group_frame :
  Cell.Group.t -> Color.t -> sel:Cell.id option -> int -> Vg.image

val isect_frame :
  Gg_kit.Pgon2.t list array option -> Color.t -> int -> Vg.image

val group : Cell.Group.t -> Vg.image list
val groups :
  Cell.Group.t -> Cell.Group.t -> Cell.Group.intersections option ->
  Vg.image list

val tm_view : aspect:float -> Trackmate.t -> Box2.t * (Vg.image -> Vg.image)
val render_pdf :
  ?title:string -> ?description:string ->
  dst:Out_channel.t -> Trackmate.t -> Vg.image list -> (unit, string) result
