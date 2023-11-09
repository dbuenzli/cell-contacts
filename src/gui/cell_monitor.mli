(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type frame := int

val v :
  Observation.t option Note.signal ->
  t:Cell.Group.t option Note.signal ->
  target:Cell.Group.t option Note.signal ->
  isect:Cell.Group.intersections option Note.signal ->
  sel:Cell.id option Note.signal ->
  frame Note.signal * Cell.id Note.event * Brr.El.t
