(*---------------------------------------------------------------------------
   Copyright (c) 2022 The brr programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** WebWorker work queue (generic). *)

(** The type for work. *)
module type WORK = sig
  type 'a t
  (** The type for work returning values of type ['a]. *)

  val perform : 'a t -> 'a Fut.t
  (** [perform w] determines to the result of [w]. *)
end

(** The type for work queue. *)
module type T = sig

  type 'a work
  (** The type for work returning values of type ['a]. *)

  type t
  (** The type for work queues. *)

  val make : unit -> (t, Jv.Error.t) Fut.result
  (** [make ()] is the function to invoke to create a work queue. *)

  val main : unit -> unit
  (** [main ()] is the main function of the work queue. Typically invoked
      when {!Brr_webworkers.Worker.ami} is [true]. *)

  val send : t -> 'a work -> 'a Fut.t
  (** [send q w] is a future that determines when the work [w] on queue [q]
      as been performed. *)
end

(** Make (Work) is a work queue for [Work]. *)
module Make (Work : WORK) : T with type 'a work := 'a Work.t
