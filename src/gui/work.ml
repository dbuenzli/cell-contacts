(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Brr
open Note

module Counter = struct
  type t = { v : int S.t; set : int S.set; is_zero : bool S.t; }
  let make () =
    let v, set = S.create 0 in
    let is_zero = S.map (Int.equal 0) v in
    { v; set; is_zero }

  let incr c = c.set (S.value c.v + 1)
  let decr c = c.set (S.value c.v - 1)
  let value c = c.v
  let is_zero c = c.is_zero
end

type 'a t =
| Id : 'a -> 'a t
| Cell_group : Trackmate.t -> (Cell.Group.t, string) result t
| Cell_isect :
    Cell.Group.t * Cell.Group.t ->
    (Cell.Group.intersections * int, string) result t
| Cell_contacts :
    Cell.Contact.spec -> Cell.Contact.t list Cell.Group.data option t

let timer tag = let tag = Jstr.v tag in Console.time tag; tag

let last_isect = ref None

let worker_perform : type a. a t -> a Fut.t = function
| Id v -> Fut.return v
| Cell_group tm ->
    (* N.B. not worth it. *)
    Fut.return (Cell.Group.of_trackmate tm)
| Cell_isect (t, target) ->
    let tt = timer "Worker: intersect" in
    let isect = Cell.Group.intersections t target in
    let () = match isect with
    | Error _  -> () | Ok (i, _) -> last_isect := Some (t, target, i)
    in
    Console.time_end tt;
    Fut.return isect
| Cell_contacts spec ->
    let tt = timer "Worker: contacts" in
    let c = match !last_isect with
    | None -> None
    | Some (t, target, isects) ->
        Some (Cell.Contact.find spec ~t ~target ~isects)
    in
    Console.time_end tt;
    Fut.return c

let main_perform ?progress w = worker_perform w

module W = struct type nonrec 'a t = 'a t let perform = worker_perform end
module Queue = Work_queue.Make (W)
let queue = ref None

let send ?progress w = match !queue with
| None -> Relax.run' (fun () -> main_perform ?progress w)
| Some q -> Queue.send q w

let setup ~use_worker =
  if not use_worker then () else
  let open Fut.Syntax in
  ignore @@
  let* q = Queue.make () in
  match q with
  | Ok q -> queue := Some q; Fut.return ()
  | Error e ->
      Console.(warn [str "Could not setup worker, will do without."; e]);
      Fut.return ()

let run_worker () = Console.(log [str "Worker running!"]); Queue.main ()
