(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Printf

let pp_str fmt =
  let b = Buffer.create 20 in                         (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); Buffer.contents b in
  Format.kfprintf k ppf fmt

let assert_t msg x1 x2 =
  let cmp x y = Lz77.compare x y = 0 in
  let printer x = pp_str "%a" Lz77.pp x in
  OUnit.assert_equal ~cmp ~printer ~msg x1 x2

let rec cmp_list fn l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _ , [] -> false
  | h1::t1, h2::t2 -> fn h1 h1 && cmp_list fn t1 t2

let printer_list fn l = String.concat ", " (List.map fn l)
let cstruct_equal x y = Cstruct.to_string x = Cstruct.to_string y
let cstruct_printer x = pp_str "%S" (Cstruct.to_string x)

let assert_buf msg x1 x2 =
  let cmp = cstruct_equal in
  let printer = cstruct_printer in
  OUnit.assert_equal ~cmp ~printer ~msg x1 x2

let assert_bufs msg x1 x2 =
  let cmp = cmp_list cstruct_equal in
  let printer = printer_list cstruct_printer in
  OUnit.assert_equal ~cmp ~printer ~msg x1 x2

let fail = OUnit.assert_string

let mk x f = (x, `Quick, f)
let cs = Cstruct.of_string

let blabla = cs "blablablabla blablab"

let compress =
  let open Lz77 in
  let buffer str = Buffer (cs str) in
  let blabla () =
    assert_t "blabla" [
      buffer "bla";
      Insert (3, 3);
      Insert (6, 6);
      buffer " ";
      Insert (10, 7);
    ] (compress blabla)
  in
  let empty () = assert_t "empty" [] (compress (cs "")) in
  [
    mk "empty" empty;
    mk "blabla" blabla;
  ]

let compress_as_cstructs =
  let open Lz77 in
  let blabla () =
    assert_bufs "blabla" [
      cs "bla";
      cs "bla";
      cs "blabla";
      cs " ";
      cs "blablab"
    ] (compress_as_cstructs blabla)
  in
  [ mk "blabla" blabla ]

let decompress =
  let check msg x () = assert_buf msg x (Lz77.decompress (Lz77.compress x)) in
  let mk msg s = mk msg (check msg s) in
  [
    mk "empty"  (cs "");
    mk "blabla" blabla;
  ]

let () =
  Alcotest.run "lz77" [
    "compress"  , compress;
    "compress-c", compress_as_cstructs;
    "decompress", decompress;
  ]
