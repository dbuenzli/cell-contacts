(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Note
open Brr


val txt : ?at:At.t list -> El.cons -> string signal -> El.t
