
let _ =
  if Array.length Sys.argv <> 2 then begin
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr "  %s <pcf filename>\n" Sys.argv.(0);
    exit 1;
  end;
  Printf.printf "Opening %s\n%!" Sys.argv.(1);
  let fd = Unix.openfile Sys.argv.(1) [ Unix.O_RDONLY ] 0o0 in
  let s = Unix.fstat fd in
  let length = s.Unix.st_size in
  Printf.printf "Reading %d bytes\n%!" length;
  let ba = Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false length in
  let c = Cstruct.of_bigarray ba in
  match Pcf.of_cstruct c with
  | None ->
    failwith "Failed to detect PCF format data"
  | Some t ->
    Printf.printf "Detected PCF format data\n";
    Printf.printf "Total glyphs: %d\n" (Pcf.Glyph.number t);
    for i = 0 to Pcf.Glyph.number t - 1 do
      Printf.printf "Glyph %d:\n%!" i;
      let bitmap = Pcf.Glyph.get_bitmap t i in
      let metrics = Pcf.Glyph.get_metrics t i in
      Printf.printf "%s\n" (Pcf.Glyph.string_of_metrics metrics);
      Array.iter
        (fun row ->
          Array.iter
            (fun col ->
              print_string (if col then "XX" else "  ")
            ) row;
          print_endline ""
        ) bitmap
    done
