(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

val to_csv :
  Trackmate.t -> Cell.Group.t ->
  Cell.Contact.t list Cell.Group.data option -> string

type sel_idx := int

val of_cell_group :
  Trackmate.t -> Cell.Group.t ->
  contacts:Cell.Contact.t list Cell.Group.data option ->
  sel:Cell.id option ->
  set_sel:Cell.id Note.event ->
  Cell.id option Note.signal * Brr.El.t
