(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Note
open Brr

(** Output (generic should be [Vz_doc.Output]). *)

val image :
  ?label:Input.content ->
  size_mm:Gg.Size2.t -> ?view:Gg.Box2.t signal -> Vg.image signal -> El.t

val image_view : view:Gg.box2 Note.signal -> Vg.image Note.signal -> Brr.El.t

val image_view' : (Gg.box2 * Vg.image) Lazy.t Note.signal -> Brr.El.t
(** The image is only created on a render frame. *)

val spinner : bool signal -> El.t
val span : ?at:At.t list -> string signal -> El.t
val block : ?at:At.t list -> string signal -> El.t
