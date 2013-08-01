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

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let of_file (filename: string) =
  let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0o0 in
  finally
    (fun () ->
      let s = Unix.fstat fd in
      let length = s.Unix.st_size in
      let ba = Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false length in
      let c = Cstruct.of_bigarray ba in
      Pcf.of_cstruct c
    ) (fun () -> Unix.close fd)

