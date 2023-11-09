(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Trackmate model loading via Xmlm *)

val of_file : string -> (Trackmate.t, string) result
val of_string : ?file:string -> string -> (Trackmate.t, string) result
