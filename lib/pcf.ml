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

let (|>) a b = b a

cstruct header {
  uint8_t magic[4];
  uint32_t table_count
} as little_endian

let magic = "\001fcp"

cstruct table_entry {
  uint32_t ty;
  uint32_t format;
  uint32_t size;
  uint32_t offset
} as little_endian

cenum ty {
  PROPERTIES       = 1;
  ACCELERATORS     = 2;
  METRICS          = 4;
  BITMAPS          = 8;
  INK_METRICS      = 16;
  BDF_ENCODINGS    = 32;
  SWIDTHS          = 64;
  GLYPH_NAMES      = 128;
  BDF_ACCELERATORS = 256
} as uint32_t

let format_most_sig_byte_first = 1 lsl 2
let format_most_sig_bit_first  = 1 lsl 3
let format_compressed_metrics  = 0x100

type t = {
  whole_buffer: Cstruct.t;
  metrics: Cstruct.t;
  bitmaps: Cstruct.t;
  encodings: Cstruct.t;
  accelerators: Cstruct.t;
}

let chop_exactly (c: Cstruct.t) bytes =
  if Cstruct.len c mod bytes <> 0
  then failwith (Printf.sprintf "buffer has length %d, not a multiple of %d" (Cstruct.len c) bytes);
  let rec loop acc rest =
    if Cstruct.len rest = 0
    then List.rev acc
    else
      let this, rest = Cstruct.split rest bytes in
      loop (this :: acc) rest in
  loop [] c

let find_table (x: Cstruct.t) desired_ty =
  let n = Int32.to_int (get_header_table_count x) in
  let table_start = Cstruct.shift x sizeof_header in
  let rec loop rest = function
    | -1 -> None
    | n ->
      let table, rest = Cstruct.split rest sizeof_table_entry in
      let ty = get_table_entry_ty table in
      match int_to_ty ty with
      | None -> loop rest (n - 1)
      | Some ty' when ty' = desired_ty -> 
        let e = Cstruct.shift x (Int32.to_int (get_table_entry_offset table)) in
        Some e
      | Some _ -> loop rest (n - 1) in
  loop table_start n

let of_cstruct (x: Cstruct.t) =
  try
    let metrics = match find_table x METRICS with
    | Some x -> x
    | None -> raise Not_found in
    let bitmaps = match find_table x BITMAPS with
    | Some x -> x
    | None -> raise Not_found in
    let encodings = match find_table x BDF_ENCODINGS with
    | Some x -> x
    | None -> raise Not_found in
    let accelerators = match find_table x ACCELERATORS with
    | Some x -> x
    | None -> raise Not_found in
    let whole_buffer = x in
    Some { whole_buffer; metrics; bitmaps; encodings; accelerators }
  with Not_found -> None

let get_uint16 format =
  if (Int32.to_int format) land format_most_sig_byte_first <> 0
  then Cstruct.BE.get_uint16
  else Cstruct.LE.get_uint16

let get_uint32 format =
  if (Int32.to_int format) land format_most_sig_byte_first <> 0
  then Cstruct.BE.get_uint32
  else Cstruct.LE.get_uint32

module Encoding = struct
  type t = int

  let of_int x = x

  let lookup_glyph t x =
    let e = t.encodings in
    let format = Cstruct.LE.get_uint32 e 0 in
    let min_char_or_byte2   = get_uint16 format e 4 in
    let max_char_or_byte2   = get_uint16 format e 6 in
    let min_byte1           = get_uint16 format e 8 in
    let max_byte1           = get_uint16 format e 10 in
    let (* default_char *)_ = get_uint16 format e 12 in
    let table = Cstruct.shift e 14 in
    let low = x land 0xff in
    let high = (x lsr 8) land 0xff in
    if x lsr 16 <> 0
    then None (* more than 16-bits *)
    else
      if high < min_byte1 || high > max_byte1
      || low < min_char_or_byte2
      || low > max_char_or_byte2
      then None
      else
        let idx = (high - min_byte1) * (max_char_or_byte2 - min_char_or_byte2 + 1) + low - min_char_or_byte2 in
        Some (get_uint16 format table (idx * 2))
end

module Metrics = struct

  type t = {
    left_side_bearing: int;
    right_side_bearing: int;
    character_width: int;
    character_ascent: int;
    character_descent: int
  }

  let to_string t =
    Printf.sprintf "{ left_side_bearing=%d; right_side_bearing=%d; character_width=%d; character_ascent=%d; character_descent=%d }"
      t.left_side_bearing t.right_side_bearing
      t.character_width t.character_ascent t.character_descent

  let uncompressed format e =
    let left_side_bearing  = get_uint16 format e 0 in
    let right_side_bearing = get_uint16 format e 2 in
    let character_width    = get_uint16 format e 4 in
    let character_ascent   = get_uint16 format e 6 in
    let character_descent  = get_uint16 format e 8 in
    { left_side_bearing; right_side_bearing; character_width;
      character_ascent; character_descent }

  let sizeof_uncompressed = 16 * 6

  let compressed e =
    let left_side_bearing  = Cstruct.get_uint8 e 0 - 0x80 in
    let right_side_bearing = Cstruct.get_uint8 e 1 - 0x80 in
    let character_width    = Cstruct.get_uint8 e 2 - 0x80 in
    let character_ascent   = Cstruct.get_uint8 e 3 - 0x80 in
    let character_descent  = Cstruct.get_uint8 e 4 - 0x80 in
    { left_side_bearing; right_side_bearing; character_width;
      character_ascent; character_descent }

  let sizeof_compressed = 5
end

type direction = LeftToRight | RightToLeft

let string_of_direction = function
  | LeftToRight -> "LeftToRight"
  | RightToLeft -> "RightToLeft"

module Accelerator = struct
  type t = {
    no_overlap: bool;
    constant_metrics: bool;
    terminal_font: bool;
    constant_width: bool;
    ink_inside: bool;
    ink_metrics: bool;
    draw_direction: direction;
    font_ascent: int;
    font_descent: int;
    min_bounds: Metrics.t;
    max_bounds: Metrics.t;
  }

  let to_string t =
    Printf.sprintf "{ no_overlap=%b constant_metrics=%b terminal_font=%b constant_width=%b ink_inside=%b ink_metrics=%b draw_direction=%s font_ascent=%d font_descent=%d min_bounds=%s max_bounds=%s }"
      t.no_overlap t.constant_metrics t.terminal_font t.constant_width
      t.ink_inside t.ink_metrics (string_of_direction t.draw_direction)
      t.font_ascent t.font_descent
      (Metrics.to_string t.min_bounds) (Metrics.to_string t.max_bounds)

end

let get_accelerator t =
  let e = t.accelerators in
  let format = Cstruct.LE.get_uint32 e 0 in
  let no_overlap       = Cstruct.get_uint8 e 4 <> 0 in
  let constant_metrics = Cstruct.get_uint8 e 5 <> 0 in
  let terminal_font    = Cstruct.get_uint8 e 6 <> 0 in
  let constant_width   = Cstruct.get_uint8 e 7 <> 0 in
  let ink_inside       = Cstruct.get_uint8 e 8 <> 0 in
  let ink_metrics      = Cstruct.get_uint8 e 9 <> 0 in
  let draw_direction   = match Cstruct.get_uint8 e 10 with
    | 0 -> LeftToRight
    | 1 -> RightToLeft
    | n -> failwith (Printf.sprintf "illegal draw direction value: %d" n) in
  (* padding *)
  let font_ascent = Int32.to_int (get_uint32 format e 12) in
  let font_descent = Int32.to_int (get_uint32 format e 16) in
  (* max_overlap *)
  let m = Cstruct.shift e 24 in
  let min_bounds = Metrics.uncompressed format m in
  let m = Cstruct.shift m Metrics.sizeof_uncompressed in
  let max_bounds = Metrics.uncompressed format m in
  { Accelerator.no_overlap; constant_metrics; terminal_font; constant_width;
    ink_inside; ink_metrics; draw_direction; font_ascent; font_descent;
    min_bounds; max_bounds }

module Glyph = struct

let total_metrics (t: t) =
  let e = t.metrics in
  let format = Cstruct.LE.get_uint32 e 0 in
  if (Int32.to_int format) land format_compressed_metrics = 0
  then Int32.to_int (get_uint32 format e 4)
  else get_uint16 format e 4

let metrics (t: t) n =
  let e = t.metrics in
  let format = Cstruct.LE.get_uint32 e 0 in
  if (Int32.to_int format) land format_compressed_metrics = 0 then begin
    let e = Cstruct.shift e (4 + 4 + Metrics.sizeof_uncompressed * n) in
    Metrics.uncompressed format e
  end else begin
    let e = Cstruct.shift e (4 + 2 + Metrics.sizeof_compressed * n) in
    Metrics.compressed e
  end 

let total_bitmaps (t: t) =
  let e = t.bitmaps in
  let format = Cstruct.LE.get_uint32 e 0 in
  Int32.to_int (get_uint32 format e 4)

let string_to_bool_array nbits msb_first x =
  let result = Array.create nbits false in
  for i = 0 to nbits - 1 do
    let byte = int_of_char x.[i / 8] in
    let bit_idx = 1 lsl (i mod 8) in
    result.(nbits - 1 - i) <- byte land bit_idx <> 0
  done;
  result

let bitmap (t: t) n =
  let e = t.bitmaps in
  let format = Cstruct.LE.get_uint32 e 0 in
  let glyph_count = Int32.to_int (get_uint32 format e 4) in
  let offsets = Cstruct.shift e 8 in
  let bitmapSizes = Cstruct.shift offsets (4 * glyph_count) in
  let bitmap_data = Cstruct.shift bitmapSizes 16 in
  let bitmapSizes = Array.map Int32.to_int [|
    get_uint32 format bitmapSizes 0;
    get_uint32 format bitmapSizes 4;
    get_uint32 format bitmapSizes 8;
    get_uint32 format bitmapSizes 12;
  |] in
  let offset = Int32.to_int (get_uint32 format offsets (4 * n)) in
  let format = Int32.to_int format in
  let total_bytes = bitmapSizes.(format land 3) in
  let len = total_bytes / glyph_count in
  let bitmap = Cstruct.sub bitmap_data offset len in
  let row_is_padded_to = 8 lsl (format land 3) in (* bits *)
  (* TODO: (format lsr 4) land 3 = 1 implies bits are stored in 16s, 2 implies 32s *)
  let m = metrics t n in
  let width_bits = m.Metrics.character_width in
  if width_bits <= 0 then begin
    Printf.printf "glyph %d has width < 0 %d\n%!" n width_bits;
    Cstruct.hexdump bitmap;
    [| |]
  end else begin
  let padding_bits =
    if width_bits mod row_is_padded_to = 0
    then 0
    else row_is_padded_to - (width_bits mod row_is_padded_to) in
  let width_bytes = (width_bits + padding_bits) / 8 in
  let msb_first = format land format_most_sig_bit_first <> 0 in
  chop_exactly bitmap width_bytes
  |> List.map Cstruct.to_string
  |> List.map (string_to_bool_array width_bits msb_first)
  |> Array.of_list
  end

type table = { ty: ty }

let is_pcf (x: Cstruct.t) = Cstruct.to_string (get_header_magic x) = magic

let number = total_bitmaps

let get_bitmap t enc = match Encoding.lookup_glyph t enc with
  | None -> None
  | Some x -> Some (bitmap t x)

let get_metrics t enc = match Encoding.lookup_glyph t enc with
  | None -> None
  | Some x -> Some (metrics t x)
end
(*
let get_tables (x: Cstruct.t) =
  let n = Int32.to_int (get_header_table_count x) in
  let table_start = Cstruct.shift x sizeof_header in
  let rec loop acc rest = function
    | -1 -> List.rev acc
    | n ->
      let table, rest = Cstruct.split rest sizeof_table_entry in
      let ty = get_table_entry_ty table in
      match int_to_ty ty with
      | None -> loop acc rest (n - 1)
      | Some METRICS ->
        let e = Cstruct.shift x (Int32.to_int (get_table_entry_offset table)) in
        let format = Int32.to_int (Cstruct.LE.get_uint32 e 0) in
        (* TODO: handle endianness options *)
        if format land format_compressed_metrics = 0 then begin
          let metrics_count = Int32.to_int (Cstruct.BE.get_uint32 e 4) in
          let left_side_bearing = Cstruct.BE.get_uint16 e 8 in
          let right_side_bearing = Cstruct.BE.get_uint16 e 10 in
          let character_width = Cstruct.BE.get_uint16 e 12 in
          let character_ascent = Cstruct.BE.get_uint16 e 14 in
          let character_descent = Cstruct.BE.get_uint16 e 16 in
        end else begin
          let metrics_count = Cstruct.BE.get_uint16 e 4 in
          let left_side_bearing = Cstruct.get_uint8 e 8 in
          let right_side_bearing = Cstruct.get_uint8 e 9 in
          let character_width = Cstruct.get_uint8 e 10 in
          let character_ascent = Cstruct.get_uint8 e 11 in
          let character_descent = Cstruct.get_uint8 e 12 in
          ()
        end;
        loop (BITMAPS :: acc) rest (n - 1)

      | Some BITMAPS ->
        let bitmap = Cstruct.shift x (Int32.to_int (get_table_entry_offset table)) in
        let format = Int32.to_int (Cstruct.LE.get_uint32 bitmap 0) in
        (* TODO: handle endianness options *)
        let glyph_count = Int32.to_int (Cstruct.BE.get_uint32 bitmap 4) in
        let offsets = Cstruct.shift bitmap 8 in
        let bitmapSizes = Cstruct.shift offsets (4 * glyph_count) in
        let bitmapSizes = Array.map Int32.to_int [|
          Cstruct.BE.get_uint32 bitmapSizes 0;
          Cstruct.BE.get_uint32 bitmapSizes 4;
          Cstruct.BE.get_uint32 bitmapSizes 8;
          Cstruct.BE.get_uint32 bitmapSizes 12;
        |] in
        for i = 0 to glyph_count - 1 do
          let offset = Int32.to_int (Cstruct.BE.get_uint32 offsets (4 * i)) in
          let len = bitmapSizes.(format land 3) / 8 in
          let glyph = Cstruct.sub x offset len in
          Printf.printf "glyph %d (length %d)\n%!" i len;
          Cstruct.hexdump glyph
        done;
        loop (BITMAPS :: acc) rest (n - 1)
      | Some t -> loop (t :: acc) rest (n - 1) in
  loop [] table_start n
*)
