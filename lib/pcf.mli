
type t
(** Represents a valid PCF format file *)

val of_cstruct: Cstruct.t -> t option
(** [of_cstruct c] evaluates to (Some t) if [c] has PCF format
    data, and None otherwise *)

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

  val get_bitmap: t -> int -> bool array array
  (** [get_bitmap t n] returns the bitmap for glyph [n] *)

  val get_metrics: t -> int -> metrics
  (** [get_metrics t n] returns the metrics for glyph [n] *)

end
