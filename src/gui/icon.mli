(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** SVG Icons.

    Taken from {{:https://heroicons.com/}heroicons}. *)

(** {1:outline Outline} *)

open Brr

val arrows_pointing_out : unit -> El.t
val arrow_path : unit -> El.t
val database : unit -> El.t
val document_arrow_down : unit -> El.t
val clock : unit -> El.t
val cube_transparent : unit -> El.t
val square_2_stack : unit -> El.t

val duplicate : unit -> El.t
val eye : unit -> El.t
val eye_off : unit -> El.t
val folder_arrow_down : unit -> El.t
val minus_circle : unit -> El.t
val pencil : unit -> El.t
val pencil_alt : unit -> El.t
val plus : unit -> El.t
val plus_circle : unit -> El.t
val plus_sm : unit -> El.t
val save_as : unit -> El.t
val tag : unit -> El.t
val trash : unit -> El.t
val view_grid_add : unit -> El.t
val x_circle : unit -> El.t

(** {1:solid Solid} *)

val solid_plus_circle : unit -> El.t
val solid_view_grid_add : unit -> El.t
val solid_x_circle : unit -> El.t
