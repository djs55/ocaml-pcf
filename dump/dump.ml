
let _ =
  if Array.length Sys.argv <> 3 then begin
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr "  %s <pcf filename> <encoding>\n" Sys.argv.(0);
    exit 1;
  end;
  Printf.printf "Opening %s\n%!" Sys.argv.(1);
  match Pcf_unix.of_file Sys.argv.(1) with
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
      Array.iteri
        (fun row_number row ->
          Printf.printf "%3d: " row_number;
          Array.iter
            (fun col ->
              print_string (if col then "X" else " ")
            ) row;
          print_endline ""
        ) bitmap
    end
