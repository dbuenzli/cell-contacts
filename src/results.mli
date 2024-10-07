(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a enc =
  { csv : Buffer.t -> 'a -> unit; (* Encoding to CSV. *)
    td : 'a -> string (* Rendering to HTML td element. *) }

type 'a col =
  { name : string; (* name for CSV column. *)
    name_th : string; (* If "" [name] is used. *)
    href : string option; (* Link to trackmate docs definition, if any. *)
    enc : 'a enc;
    get :
      Trackmate.t ->
      Cell.t -> Trackmate.track -> Cell.Contact.t list option -> 'a }

type ecol = C : 'a col -> ecol

val cols : ecol list

val to_csv :
  Trackmate.t -> Cell.Group.t ->
  Cell.Contact.t list Cell.Group.data option -> string
