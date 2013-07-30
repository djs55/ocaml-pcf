
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
  if Pcf.is_pcf c
  then Printf.printf "Detected PCF format data\n"
  else failwith "Failed to detect PCF format data";
  let tables = Pcf.get_tables c in
  List.iteri (fun i table ->
    Printf.printf "Table %d / %d:\n%!" i (List.length tables);
    Printf.printf "  %s\n%!" (Pcf.ty_to_string table);
  ) tables

