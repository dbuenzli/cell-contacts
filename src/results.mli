(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Generate table results. *)

type 'a enc =
  { csv : Buffer.t -> 'a -> unit; (** Encoding to CSV. *)
    td : 'a -> string (** Rendering to HTML td element. *) }
(** The type for column encoder. *)

type 'a col =
  { name : string; (** name for CSV column. *)
    name_th : string; (** If "" [name] is used. *)
    href : string option; (** Link to trackmate docs definition, if any. *)
    enc : 'a enc; (** encoder *)
    get :
      Observation.t ->
      Trackmate.t ->
      Cell.t -> Trackmate.track -> Cell.Contact.t option option -> 'a }
(** The type for column descriptions. How to encode and how to get
    it from the data. *)

type ecol = C : 'a col -> ecol
(** The type for existential column descriptions. *)

val cols : ecol list
(** [cols] are the columns of the results table. *)

val to_csv :
  headers:bool ->
  obs:Observation.t -> t:Cell.Group.t ->
  contacts:Cell.Contact.t option Cell.Group.data option -> string
(** [to_csv] renders the results to a CSV file. *)

val contact_distances_to_csv :
  normalize:bool -> headers:bool -> obs:Observation.t -> t:Cell.Group.t ->
  contacts:Cell.Contact.t option Cell.Group.data -> string

val contact_distances_to_json_objs :
  normalize:bool -> obs:Observation.t -> t:Cell.Group.t ->
  contacts:Cell.Contact.t option Cell.Group.data -> string list
