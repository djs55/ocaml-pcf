(*
 * Copyright (c) 2013 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type t
(** Represents a valid PCF format file *)

val of_cstruct: Cstruct.t -> t option
(** [of_cstruct c] evaluates to (Some t) if [c] has PCF format
    data, and None otherwise *)

module Encoding : sig
  type t

  val of_int: int -> t
end

module Glyph : sig

  type metrics = {
    left_side_bearing: int;
    right_side_bearing: int;
    character_width: int;
    character_ascent: int;
    character_descent: int
  }
  (** Metrics for a specific glyph *)

  val string_of_metrics: metrics -> string
  (** Pretty-print a metrics value *)

  val number: t -> int
  (** [number t] returns the number of glyphs *)

  val get_bitmap: t -> Encoding.t -> bool array array option
  (** [get_bitmap t n] returns the bitmap for glyph [n] *)

  val get_metrics: t -> Encoding.t -> metrics option
  (** [get_metrics t n] returns the metrics for glyph [n] *)

end
