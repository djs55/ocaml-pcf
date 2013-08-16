Pure OCaml parser for X11 PCF bitmap files
==========================================

What is PCF? To quote
[wikipedia](http://en.wikipedia.org/wiki/Portable_Compiled_Format)

> Portable Compiled Format (PCF) is a bitmap font format used by Xserver
> in its core font system, and has been used for decades. PCF fonts are
> usually installed, by default, on most Unix based operating systems,
> and are used in terminals such as xterm.

If you still want bitmap fonts (as opposed to rasterised scalable fonts)
then X11's PCF fonts are reasonable.

Example
-------

Using the test CLI:

```
cp /usr/share/fonts/X11/misc/9x15.pcf.gz .
gunzip 9x15.pcf.gz 

./dump.native 9x15.pcf 56
```

Example
-------

Using the utop toplevel:

```
utop # #require "pcf.unix";;
/usr/lib/ocaml/threads: added to search path
/local/scratch/djs/.opam/system/lib/ocplib-endian: added to search path
/local/scratch/djs/.opam/system/lib/ocplib-endian/ocplib_endian.cma: loaded     
/local/scratch/djs/.opam/system/lib/ocplib-endian/bigstring.cma: loaded
/local/scratch/djs/.opam/system/lib/cstruct: added to search path
/local/scratch/djs/.opam/system/lib/cstruct/cstruct.cma: loaded
/local/scratch/djs/.opam/system/lib/pcf: added to search path
/local/scratch/djs/.opam/system/lib/pcf/pcf.cma: loaded
/local/scratch/djs/.opam/system/lib/pcf/pcf_unix.cma: loaded

utop # let font = match Pcf_unix.of_file "9x15.pcf" with
       | None -> failwith "not a readable PCF file"
       | Some pcf -> pcf;;
val font : Pcf.t = <abstr>

utop # let e = Pcf.Encoding.of_int 56;;
val e : Pcf.Encoding.t = <abstr>

utop # let g = match Pcf.Glyph.get_bitmap font e with
       | None -> failwith "glyph not defined" 
       | Some g -> g;;
val g : bool array array =
  [|[|false; false; false; false; false; false; false; false; false|];
    [|false; false; false; false; false; false; false; false; false|];          
    [|false; false; false; false; true; true; true; false; false|];
    [|false; false; false; true; false; false; false; true; false|];
    [|false; false; true; false; false; false; false; false; true|];
    [|false; false; false; true; false; false; false; true; false|];
    [|false; false; false; false; true; true; true; false; false|];
    [|false; false; false; true; false; false; false; true; false|];
    [|false; false; true; false; false; false; false; false; true|];
    [|false; false; true; false; false; false; false; false; true|];
    [|false; false; false; true; false; false; false; true; false|];
    [|false; false; false; false; true; true; true; false; false|];
    [|false; false; false; false; false; false; false; false; false|];
    [|false; false; false; false; false; false; false; false; false|];
    [|false; false; false; false; false; false; false; false; false|]|]

utop # Array.iter (fun row ->
         Array.iter (fun x -> print_string (if x then "*" else " ")) row;
         print_endline ""
       ) g;;

    ***
   *   * 
  *     *
   *   * 
    ***  
   *   * 
  *     *
  *     *
   *   * 
    ***  


- : unit = ()
```

References
----------

The PCF format is described on
[font forge](http://fontforge.org/pcf-format.html)
