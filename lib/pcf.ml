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
    let whole_buffer = x in
    Some { whole_buffer; metrics; bitmaps }
  with Not_found -> None

module Glyph = struct

type metrics = {
  left_side_bearing: int;
  right_side_bearing: int;
  character_width: int;
  character_ascent: int;
  character_descent: int
}

let string_of_metrics m =
  Printf.sprintf "{ left_side_bearing=%d; right_side_bearing=%d; character_width=%d; character_ascent=%d; character_descent=%d }"
    m.left_side_bearing m.right_side_bearing
    m.character_width m.character_ascent m.character_descent

let total_metrics (t: t) =
  let e = t.metrics in
  let format = Int32.to_int (Cstruct.LE.get_uint32 e 0) in
  (* TODO: handle endianness options *)
  if format land format_compressed_metrics = 0
  then Int32.to_int (Cstruct.BE.get_uint32 e 4)
  else Cstruct.BE.get_uint16 e 4

let metrics (t: t) n =
  let e = t.metrics in
  let format = Int32.to_int (Cstruct.LE.get_uint32 e 0) in
  (* TODO: handle endianness options *)
  if format land format_compressed_metrics = 0 then begin
    let sizeof_uncompressed = 16 * 6 in
    let e = Cstruct.shift e (4 + 4 + sizeof_uncompressed * n) in
    let left_side_bearing = Cstruct.BE.get_uint16 e 0 in
    let right_side_bearing = Cstruct.BE.get_uint16 e 2 in
    let character_width = Cstruct.BE.get_uint16 e 4 in
    let character_ascent = Cstruct.BE.get_uint16 e 6 in
    let character_descent = Cstruct.BE.get_uint16 e 8 in
    { left_side_bearing; right_side_bearing; character_width;
      character_ascent; character_descent }
  end else begin
    let sizeof_compressed = 5 in
    let e = Cstruct.shift e (4 + sizeof_compressed * n) in
    let left_side_bearing = Cstruct.get_uint8 e 0 - 0x80 in
    let right_side_bearing = Cstruct.get_uint8 e 1 - 0x80 in
    let character_width = Cstruct.get_uint8 e 2 - 0x80 in
    let character_ascent = Cstruct.get_uint8 e 3 - 0x80 in
    let character_descent = Cstruct.get_uint8 e 4 - 0x80 in
    { left_side_bearing; right_side_bearing; character_width;
      character_ascent; character_descent }
  end 

let total_bitmaps (t: t) =
  let e = t.bitmaps in
  (* TODO: handle endianness options *)
  Int32.to_int (Cstruct.BE.get_uint32 e 4)

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
  let format = Int32.to_int (Cstruct.LE.get_uint32 e 0) in
  (* TODO: handle endianness options *)
  let glyph_count = Int32.to_int (Cstruct.BE.get_uint32 e 4) in
  let offsets = Cstruct.shift e 8 in
  let bitmapSizes = Cstruct.shift offsets (4 * glyph_count) in
  let bitmap_data = Cstruct.shift bitmapSizes 16 in
  let bitmapSizes = Array.map Int32.to_int [|
    Cstruct.BE.get_uint32 bitmapSizes 0;
    Cstruct.BE.get_uint32 bitmapSizes 4;
    Cstruct.BE.get_uint32 bitmapSizes 8;
    Cstruct.BE.get_uint32 bitmapSizes 12;
  |] in
  let offset = Int32.to_int (Cstruct.BE.get_uint32 offsets (4 * n)) in
  let total_bytes = bitmapSizes.(format land 3) in
  let len = total_bytes / glyph_count in
  let bitmap = Cstruct.sub bitmap_data offset len in
  let row_is_padded_to = 8 lsl ((format lsr 4) land 3) in (* bits *)
  let m = metrics t n in
  let width_bits = m.left_side_bearing + m.right_side_bearing in
  if width_bits <= 0 then begin
    Printf.printf "glyph %d has width < 0 %d\n%!" n width_bits;
    Cstruct.hexdump bitmap;
    [| |]
  end else begin
  let width_bytes = (width_bits + (width_bits mod row_is_padded_to)) / 8 in
  let msb_first = format land format_most_sig_bit_first <> 0 in
  chop_exactly bitmap width_bytes
  |> List.map Cstruct.to_string
  |> List.map (string_to_bool_array width_bits msb_first)
  |> Array.of_list
  end

type table = { ty: ty }

let is_pcf (x: Cstruct.t) = Cstruct.to_string (get_header_magic x) = magic

let number = total_bitmaps

let get_bitmap = bitmap

let get_metrics = metrics
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
