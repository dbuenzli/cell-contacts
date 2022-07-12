(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Note
open Brr

(** User input.

    This module provides simple user interface elements to input
    values from the user. *)

type content =
[ `Txt of string (** Static text. *)
| `Els of El.t list signal (** Arbitrary dynamic content. *) ]
(** The type for content. *)

(** {1:triggers Triggers} *)

val trigger :
  ?label:content -> ?enabled:bool signal -> 'a -> content -> 'a event * El.t
(** [trigger ~label ~enabled o name] is [(e, i)] with [e] an event, [i] an
      input button making [o] occur on [e] when triggered and:
      {ul
      {- [name], the content rendered on the button.}
      {- [enabled], indicates if the input button can be actuated, defaults
         to {!S.Bool.true'}.}
      {- [label], a label the input, if any.}} *)

val triggers :
  ?label:content -> ?enabled:bool signal -> init:'a ->
  (('a -> 'a) * content) list -> 'a signal * El.t
(** [triggers ~label ~enabled ~init triggers] is [(s, i)] with [s] a signal
      starting with [init], [i] a list of input buttons changing [s] when
      triggered and:
      {ul
      {- [triggers], for each button an accumulation function
         called when the button is triggered and the content rendered on it.}
      {- [enabled], indicates if the input button can be actuated, defaults
         to {!S.Bool.true'}.}
      {- [label], a label for the input, if any.}} *)

(** {1:bool Booleans} *)

val bool :
  ?label:content -> ?enabled:bool signal -> init:bool -> content ->
  bool signal * El.t
(** [bool ~label ~enabled ~init name] is [(s, i)] with [s] a [bool] signal
      starting with [init], [i] an input checkbox changing [s] and:
      {ul
      {- [name], the content rendered on the right of the checkbox.}
      {- [enabled], indicates whether the input can be actuated,
         defaults to {!S.Bool.true'}.}
      {- [label], a label for the input, if any.}} *)

(** {1:numbers Numbers} *)

type number_affordance =
  [ `Slider (** Slider only. *)
  | `Text_and_slider  (** Text field and slider. *)
  | `Text (** Text field only. *) ]
(** The type for specifying number input affordances. *)

val int :
  ?on_change:bool ->
  ?at:At.t list ->
  ?label:content -> ?text_size:int -> ?enabled:bool signal ->
  ?affordance:number_affordance ->
  ?min:int signal -> ?max:int signal -> ?step:int signal ->
  int -> int signal * El.t
(** [int ~label ~enabled ~affordance ~min ~max ~step init] is [(s, i)]
      with [s] an [int] signal starting with [init], [i] an input changing
      [s] and:
      {ul
      {- [min] the minimal admissible value, defaults to [S.const 0]}
      {- [max] the maximal admissible value, defaults to [S.const 100]}
      {- [step] the slider step increment, if unspecified the slider
         is continuously updated.}
      {- [affordance] indicates which kind of inputs are available for
         specifying the number. By default this is [`Text_and_slider],
         unless the initial value of [min] or [max] is respectively
         [Int.min_int] or [Int.max_int].}
      {- [enabled], indicates whether the input can be actuated,
         defaults to {!S.Bool.true'}.}
      {- [label], a label for the input, if any.}} *)

val float :
  ?on_change:bool ->
  ?at:At.t list -> ?label:content -> ?text_size:int -> ?enabled:bool signal ->
  ?affordance:number_affordance ->
  ?min:float signal -> ?max:float signal -> ?step:float signal ->
  float -> float signal * El.t
(** [float ~label ~enabled ~affordance ~min ~max ~step init] is [(s, i)]
      a [float] signal starting with [init], [i] an input changing [s] and:
      {ul
      {- [min] the minimal admissible value, defaults to [S.const 0.]}
      {- [max] the maximal admissible value, defaults to [S.const 1.]}
      {- [step] the slider step increment, if unspecified the slider
         is continuously updated.}
      {- [affordance] indicates which kind of inputs are used for
         specifying the number. By default this is [`Text_and_slider],
         unless the initial value of [max] or [min] is not
         {!Float.is_finite}.}
      {- [enabled], indicates whether the input can be actuated,
         defaults to {!S.Bool.true'}.}
      {- [label], a label for the input, if any.}} *)

(** {1:enums Enumerations} *)

type enum_affordance =
  [ `Buttons (** Checkboxes or radio buttons *)
  | `Menu (** Menu or cells. *) ]
(** The type for specifying value enumeration affordances. *)

val one_of :
  ?label:content -> ?enabled:bool signal -> ?affordance:enum_affordance ->
  ?eq:('a -> 'a -> bool) -> 'a list -> init:'a -> ('a -> content) ->
  'a signal * El.t
(** [one_of ~label ~enabled ~affordance ~eq enum ~init names] is
      [(s, i)] with [s] a signal starting with [init], [i] an input
      changing [s] to one of the values of [enum] and:
      {ul
      {- [names e] is the content rendered for an element [e] of
         [enum].}
      {- [eq] is a comparison function for elements of the enumeration,
         defaults to {!Stdlib.( = )}.}
      {- [affordance] indicates which kind of inputs are used for specifying
         the value. [`Button] uses radio boxes and is the default if [enum]
         has up to five elements. [`Menu] uses a drop down menu and is the
         default if [enum] has more than five elements.}
      {- [enabled], indicates whether the input can be actuated,
         defaults to {!S.Bool.true'}.}
      {- [label], a label for the input, if any.}} *)

val list :
  ?label:content -> ?enabled:bool signal -> ?affordance:enum_affordance ->
  ?eq:('a -> 'a -> bool) -> 'a list -> init:'a list -> ('a -> content) ->
  'a list signal * El.t
(** [list ~label ~enabled enum ~init name] is [(s, i)] with [s] a
      signal starting with [init], [i] an input changing [s] to a list
      of values from [enum] and:
      {ul
      {- [names e] is the content rendered for an element [e] of [enum].}
      {- [eq] is a comparison function for values of the enumeration,
         {!Stdlib.( = )}.}
      {- [affordance] indicates which kind of inputs are used for specifying
         the value. [`Button] uses check boxes and is the default if [enum]
         has up to five elements.  [`Menu] uses selectable cells and is
         the default if [enum] has more than five elements}
      {- [enabled], indicates whether the input can be actuated,
         defaults to {!S.Bool.true'}.}
      {- [label], a label for the input, if any.}} *)


(** {1:file Files} *)

type file_type = string
(** The type for specifying file types. This is either:
      {ul
      {- A file extension starting with a ['.'].}
      {- A valid MIME type.}
      {- ["audio/*"] for any audio file.}
      {- ["image/*"] for any image file.}
      {- ["video/*"] for any video file.}} *)

val file :
  ?label:content -> ?enabled:bool signal -> ?accept:file_type list ->
  content -> Brr.File.t event * El.t
(** [file ~label ~enabled ~accept name] is [(e, i)] with [e] an
      event which occurs when a file is selected, [i] an input and:
      {ul
      {- [name], the content rendered on the button.}
      {- [accept], the list of accepted file types, any if unspecified.}
      {- [enabled], indicates whether the input can be actuated,
         defaults to {!S.Bool.true'}.}
      {- [label], a label for the input, if any.}} *)

type files_select =
  [ `Files (** By selecting multiple files. *)
  | `Dir (** By selecting a single directory. *)
  | `Dirs (** By selecting multiple directories. *)]
(** The type for specifying how multiple files should be selected. *)

val files :
  ?label:content -> ?enabled:bool signal -> ?accept:file_type list ->
  select:files_select -> content -> Brr.File.t list event * El.t
(** [files ~label ~enabled ~accept ~select name] is [(e, i)] with [e] an
      event which occurs when files are selected, [i] an input and:
      {ul
      {- [name], the content rendered on the button.}
      {- [select], the way multiple files are selected.}
      {- [accept] a list of accepted file types, any if unspecified.
         {b Warning} this seems ignored by browsers for directory selection.}
      {- [enabled], indicates whether the input can be actuated,
         defaults to {!S.Bool.true'}.}
      {- [label], a label for the input, if any.}} *)

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The vz programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
