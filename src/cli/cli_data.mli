(*---------------------------------------------------------------------------
   Copyright (c) 2022 The cell programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Data loader for the command line. *)

open B0_std

val load_observations : Filepath.t -> (Observation.t list, string) result
(** [load_observations dir] loads observations from the directory [dir]. *)
