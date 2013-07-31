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

type table = { ty: ty }

let is_pcf (x: Cstruct.t) = Cstruct.to_string (get_header_magic x) = magic

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
          Printf.printf "metrics_count = %d\n%!" metrics_count
        end else begin
          let metrics_count = Cstruct.BE.get_uint16 e 4 in
          Printf.printf "metrics_count = %d\n%!" metrics_count;
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
Printf.printf "glyph_count=%d\n%!" glyph_count;
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

