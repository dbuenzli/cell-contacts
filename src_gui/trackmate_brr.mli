(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Trackmate model loading via browser XML parser *)

val of_jstr : ?file:Jstr.t -> Jstr.t -> (Trackmate.t, Jv.Error.t) result
