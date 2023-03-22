(*---------------------------------------------------------------------------
   Copyright (c) 2022 The negsp programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Design ground.

    Provides a few elements to quickly bootstrap and work with a
    design. This is meant to be tweaked and extended along with a
    carefully nurtured and minimal CSS file.

    The system broadly distinguishes layout and looks. Layout is the
    division of space into blocks. Looks are the aesthetics of the
    content filling the blocks. Both do interact to provide the feel.

    The system uses
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Logical_Properties}
    logical dimension mappings} as it accommodates different
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/writing-mode}
    writing modes}. To get it into your head, keep this in mind:

    {ul
    {- The {e block dimension} is perpendicular to the flow of text.
       For latin scripts, this is the vertical axis, {e start} means
       top, {e end} means bottom and {e size} means height. For Japanese
       this is the horizontal axis, {e start} means right, {e end}
       means left and {e size} means width.}
    {- The {e inline dimension} is parallel to the flow of text. For
       latin scripts, this is the horizontal axis, {e start} means
       left, {e end} means right and {e size} means width. For Japanese
       this is the vertical axis, {e start} means top, {e end} means bottom
       and {e size} means height.}} *)

(** {1:layout Layout} *)

type at = Brr.At.t

(** Design sizes. *)
module Size : sig

  type t
  (** The type for sizes. This represents a
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/length}CSS length}
      value. *)

  val zero : t
  (** [zero] is [0]. *)

  val thin : t
  (** [thin] is [1px]. *)

  (** {1:space Space} *)

  type space = [ `XXS | `XS | `S | `M | `L | `XL | `XXL | `XXXL | `XXXXL ]
  (** The type for spaces. *)

  val space : space -> t
  (** [space sp] is the given space. *)

  val space_minmax : space -> space -> t
  (** [space_minmax min max] is [min] at the minimal design space and
      max at the maximal design space. *)

  (** {1:gap Gaps} *)

  type gap = [ `Zero | `Sp of space | `Minmax of space * space | `Size of t ]
  (** The type for specifying gaps. *)

  val gap : gap -> t
  (** [gap g] is a size from the given gap. *)

  (** {1:measure Measure} *)

  type measure = [ `XS | `S | `M | `L | `XL ]
  (** The type for logical measure sizes. *)

  val measure : measure -> t
  (** [measure] is a size of the given logical measure. *)

  (** {1:other Other} *)

  val leading_m : t
  (** The [--leading_m] variable. Normally this should be equal to
      [space `M]. *)

  val measure_m : t
  (** The [--measure_m] variable. *)

  (** {1:raw Raw} *)

  val raw : string -> t
  (** [raw] is a raw
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/length}CSS length}
      value. *)
end

(** Text flow and properties. *)
module Text : sig

  (** {1:block Text flow} *)

  val flow : unit -> at list
  (** A [flow] block orders its children on the block dimension and
      insert a basic gap between them as specified in the stylesheet
      which can also provide various gap overrides for example for
      headings. *)

  (** {1:size Text size} *)

  type size =
  [ `XXS (** [--font_xxs] *)
  | `XS (** [--font_xs] *)
  | `S  (** [--font_s] *)
  | `M (** [--font_m] *)
  | `L  (** [--font_l] *)
  | `XL (** [--font_xl] *)
  | `XXL (** [--font_xxl] *)
  | `Custom of Size.t * Size.t option (** [font-size] and [line-height] *)]
  (** The type for text sizes. *)

  val size : size -> at
  (** [size s] sets the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-size}[font-size]}
      and
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/line-height}
      [line-height]} according to [s]. *)

  (** {1:align Text alignment} *)

  type align =
  [ `Start (** On the start edge of the block. *)
  | `End (** On the end edge of the block. *)
  | `Center (** Center in the block. *)
  | `Justify (** Justify in the block (and activates
                 {{:https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens}
                 [hyphens:auto]}). *) ]
  (** The type for text alignment. *)

  val align : align -> at
  (** [align a] sets the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-align}
      [text-align]} property according to [a]. *)

  (** {1:leading Leading} *)

  type leading =
  [ `None (** [1] *)
  | `XS (** [--leading_xs] *)
  | `S (** [--leading_s] *)
  | `M (** [--leading_m] *)
  | `L (** [--leading_l] *)
  | `XL (** [--leading_xl] *)
  | `Custom of Size.t ]
  (** The type for leading. *)

  val leading : leading -> at
  (** [leading l] sets the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/line-height}
      [line-height]} property according to [l]. *)
end

(** Layout primitives.

    This module provides primitives for devising layouts with CSS
    based on {{:https://every-layout.dev/}Every layout} layouts.

    {b TODO}
    {ul
    {- {!Layout.val-stack} we are missing split after. But I don't think it's
       such a good idea to count. We rather want something that pushes
       two blocks at the far ends in each direction.}
    {- Sidebar, would be nice to have an option to force the side
       bar at the start or end in the block direction}
    {- Think about children vs block itself (box, center) distinctions}
    {- Should we really cater for multiple layout applications on
       the same element ?}
    {- Switcher, maybe add a few classes for defining other children
       limits (currently is 4).}
    {- Frame, add support for [object-position] for replaced elements.}
    {- Reel, the [scroll-snap] stuff should be explored a bit.
       This is perfect to make galleries etc.}
    {- Maybe eventually switch to less poetic but more
       indicative names}} *)
module Layout : sig

  type align = [ `Start | `End | `Center | `Baseline ]

  type justify =
  [ `Start | `End | `Center | `Space_between | `Space_around
  | `Space_evenly | `Stretch ]

(** {1:block_layouts Block layouts}

    {{:https://every-layout.dev/layouts/}Visual reference}. *)

  (** {2:stack Stack} *)

  val stack : ?gap:Size.gap -> unit -> at list
  (** A [stack] block orders its children on the block direction and inserts
      a gap between them.
      {ul
      {- [gap] specifies the gap between children. Defaults to
         [`Sp `M].}} *)

  (** {2:box Box} *)

  val box : ?border:Size.t -> ?pad:Size.gap -> unit -> at list
  (** A [box] block adds a uniform pad around a block and, optionally,
      highlights it by a border.
      {ul
      {- [pad] specifies the padding. Defaults to [`Sp `M].}
      {- [border] specifies the border width.
         Defaults to {!Size.zero} FIXME move to look}} *)

  (** {2:center Center} *)

  val center :
    ?children:bool -> ?pad:Size.gap -> ?max_size:Size.t -> unit -> at list
  (** A [center] block centers it in its parent on the inline direction.
      {ul
      {- [pad], defines padding for the block.}
      {- [max_size], is the maximal inline size of the block. Defaults to
         {!Size.val-measure}}
      {- [children], if [true] centers children narrower than the parent
         block's inline size.}} *)

  (** {2:cluster Cluster} *)

  val cluster :
    ?gap:Size.gap -> ?align:align -> ?justify:justify -> unit -> at list
  (** [cluster] block orders its children in the inline direction,
      inserts a gap between them and wraps them if needed. Parameters
      include:
      {ul
      {- [gap] is the space between elements in the inline and block
         directions. Defaults to [`Sp `M].}
      {- [align] for the alignement perpendicular to the inline direction.
          Defaults to [`Center].}
      {- [justify] for justification relative to the block direction.
         Defaults to [`Start].}} *)

  (** {2:sidebar Sidebar} *)

  val sidebar :
    ?gap:Size.gap -> ?bar_size:Size.t -> ?min_content:int -> [`Start | `End] ->
    unit -> at list
  (** A [sidebar bar] block places two children next to each other
      separated by a gap. When the available inline space becomes
      too small the first block is pushed above. Parameters include:
      {ul
      {- [bar] indicates if the bar is at the start or the end
         of the inline. And the block direction it gets pushed
         on the collapse}
      {- [gap] is the space between the bar and the content, both
         in the inline and block direction. Defaults to [`Sp `M].}
      {- [min_content] is the minimal inline size of the content
         before the collapse expressed as percentage of the parent's
         inline size.}}

      {b Warning.} Suffers from the self-nest quirk. *)

  (** {2:switcher Switcher} *)

  val switcher : ?gap:Size.gap -> ?at_size:Size.t -> unit -> at list
  (** A [switcher] switches its children between inline layout to a stack
      layout when the container size becomes smaller than [at_size]. Or if
      it has more than 4 elements (XXX we could add a few more options).
      {ul
      {- [gap] is the space between elements in the inline and
         block direction. Defaults to [`Sp `M].}
      {- [at_size] is the size at which the layout changes.
         Defaults to {!Size.val-measure}.}}

      {b Warning.} Suffers from the self-nest quirk. *)

  (** {2:cover Cover} *)

  val cover : ?gap:Size.gap -> ?min_size:Size.t -> unit -> at list
  (** A [cover] block centers a {!central} element on the block dimension
      and pushes other elements before and after it at the beginning and
      the end of the block.
      {ul
      {- [gap] is the space between elements, except for {!central}
         where it's the minimal space.}
      {- [min_size] is the minimal block size of the cover block,
         defaults to [100vh] (the size of the viewport).}} *)

  val central : at
  (** [central] the attribute to add to the element to center. *)

  (** {2:grid Grid} *)

  val grid : ?gap:Size.gap -> ?min:Size.t -> unit -> at list
  (** A [grid] block lays out the children in identically sized
      blocks separated by [gap], it's a more rigid {!val-cluster}.
      {ul
      {- [gap] is the space between grid elements both in the inline
         and block direction.}
      {- [min] is the minimal inline size of grid items}} *)

  (** {2:frame Frame} *)

  val frame : ?aspect:(int * int) -> unit -> at list
  (** A [frame] block crops its child in the center
      to the given aspect ratio.
      {ul
      {- [aspect] is the aspect ratio, defaults to [16,9] which stands
         for [16 / 9]}} *)

  (** {2:reel Reel} *)

  val reel :
    ?scrollbar_size:Size.t -> ?scrollbar_visible:[`Auto | `Always] ->
    ?gap:Size.gap -> ?item_size:Size.t -> unit -> at list
  (** A [reel] block keeps its children on the inline direction and
      adds a scroll bar if needed.
      {ul
      {- [item_size] is an inline size for the children. If unspecified
         their size depends on their content.}
      {- [gap] is the gap betwen children.}
      {- [scrollbar_visible] indicates whether the scrollbar should
         always be visible or only added if needed}
      {- [scrollbar_size] is the block size of the scroll bar
         (defaults to [`Sp `XXXS])}} *)

  (** {2:imposter Imposter} *)

  val imposter :
    ?position:[`Ancestor|`Viewport] -> ?contained:bool -> ?pad:Size.gap ->
    unit -> at list
  (** An [imposter] surimposes itself in the center of the content.
      {ul
      {- [position], if [`Ancestor] the imposter is in the center of
         the first ancestor whose [position:] attribute is [relative].
         If [`Viewport] the imposter is in the center of the viewport.
         Defaults to [`Ancestor].}
      {- [contained], if [true] (default) the imposter does not breakout
         from its positioning element.}
      {- [pad] whenever [contain] is [true] ensures there is at least [pad]
         between the containing element and the imposter.}} *)

  (** {1:inline_layout Inline layout} *)

  (** {2:icon Icon} *)

  val icon : at
  (** [icon] sizes an icon. *)

  val with_icon : ?gap:Size.gap -> unit -> at list
  (** [with_icon ?gap ()] is for an inline whith an {!val-icon} child.
      Makes sure there is [gap] (defaults to [0.25em]) at the inline end
      of the icon. *)
end

(** {1:look Look} *)

(** Fonts. *)
module Font : sig

  (** {1:family Family} *)

  type family =
  [ `Headings (** [--font_headings] *)
  | `Body (** [--font_body] *)
  | `Mono (** [--font_mono] *)
  | `Custom of string ]
  (** The type for font families. *)

  val family : family -> at
  (** [family f] sets the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-family}
      [font-family]} to [f]. *)

  (** {1:slant Slant} *)

  type slant =
  [ `Normal
  | `Italic
  | `Oblique
  | `Custom of string ]
  (** The type for slant. *)

  val slant : slant -> at
  (** [slant s] sets the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-style}
      [font-style]} with [s]. *)

  (** {1:weight Weight} *)

  type weight =
  [ `W100 | `W200 | `W300 | `W400 | `W500 | `W600 | `W700 | `W800 | `W900 ]

  val weight : weight -> at
  (** [weight w] sets the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight}
      [font-weight]} with [w]. *)

  (** {1:stretch Stretch}

      FIXME sort-out stretch and tracking. *)

  type stretch = [ `XXS | `XS | `S | `M | `L | `XL | `XXL ]
  (** The type for font stretch. *)

  val stretch : stretch -> at
  (** [stretch] sets the [font_stretch_*] class. *)

  (** {1:numerals Numerals} *)

  type numerics =
  [ `Default (** [--font_numeric_default] *)
  | `Normal (** [normal] *)
  | `Tabular (** [--font_numeric_tabular] *)
  | `Custom of string ]

  val numerics : numerics -> at
  (** [numeric n] sets the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-numeric}
      [font-variant-numeric]} property according to [a]. *)
end

(** Borders *)
module Border : sig

  type style =
  [ `None
  | `Solid
  | `Custom of string ]

  val style : style -> at
  (** [style st] sets the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-style}
      [border-style]} according to [st]. *)

  val width : Size.t -> at
  (** [width s] sets the
      {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-width}
      [width]} to [s]. *)

  val block_start : Size.t -> at
  val block_end : Size.t -> at
  val inline_start : Size.t -> at
  val inline_end : Size.t -> at
end

(** {1:sample Sample content} *)

(** Sample content.

      Some fun could be had with a bit of random here. *)
module Sample : sig
  val text : string
  val title : string
end

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The negsp programmers

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
