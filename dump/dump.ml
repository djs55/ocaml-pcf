
let _ =
  if Array.length Sys.argv <> 3 then begin
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr "  %s <pcf filename> <encoding>\n" Sys.argv.(0);
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
    let a = Pcf.get_accelerator t in
    Printf.printf "Accelerator: %s\n%!" (Pcf.Accelerator.to_string a);
    let e = Pcf.Encoding.of_int (int_of_string Sys.argv.(2)) in
    begin match Pcf.Glyph.get_bitmap t e with
    | None ->
      Printf.printf "No glyph for encoding\n%!"
    | Some bitmap ->
      Array.iter
        (fun row ->
          Array.iter
            (fun col ->
              print_string (if col then "XX" else "  ")
            ) row;
          print_endline ""
        ) bitmap
    end
