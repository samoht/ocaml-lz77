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

type key = (char * char * char) option

let key buf i =
  if i < Cstruct.len buf - 3 then
    let x = Cstruct.get_char buf i in
    let y = Cstruct.get_char buf (i+1) in
    let z = Cstruct.get_char buf (i+2) in
    Some (x, y, z)
  else
    None

type table = (key, int list) Hashtbl.t

let find tbl x =
  try Hashtbl.find tbl x
  with Not_found -> []

let add tbl x off =
  let l = find tbl x in
  Hashtbl.replace tbl x (off :: l)

let longuest_substring buf i j =
  let rec aux acc len =
    if i + len < j  (* FIXME valid but not allowed by the original algorithm *)
    && j + len < Cstruct.len buf
    && Cstruct.get_char buf (i+len) = Cstruct.get_char buf (j+len)
    then aux (Some (len + 1)) (len+1)
    else acc
  in
  aux None 0

let default_max_window = 32 * 1024

type elt =
  | Buffer of Cstruct.t
  | Insert of int * int

let compare_elt x y = match x, y with
  | Buffer x, Buffer y ->

    (* FIXME *) String.compare (Cstruct.to_string x) (Cstruct.to_string y)
  | Insert (x, y), Insert (u, v) -> Pervasives.compare (x, y) (u, v)
  | Buffer _, _ -> 1
  | Insert _, _ -> -1

let pp_elt fmt = function
  | Buffer buf       -> Format.fprintf fmt "Buffer %S" (Cstruct.to_string buf)
  | Insert (off,len) -> Format.fprintf fmt "Insert (%d, %d)" off len

type t = elt list

let rec compare l1 l2 = match l1, l2 with
  | [], [] -> 0
  | [], _  -> -1
  | _ , [] -> 1
  | h1::t1, h2::t2 ->
    match compare_elt h1 h2 with
    | 0 -> compare t1 t2
    | i -> i

let pp fmt l =
  Format.fprintf fmt "[@[<hov 2> ";
  List.iter (Format.fprintf fmt "%a;@ " pp_elt) l;
  Format.fprintf fmt "@]]@;"

let max_insert a b =
  match a, b with
  | Some (_, x), Some (_, y) -> if x >= y then a else b
  | Some _     , None        -> a
  | None       , Some _      -> b
  | None       , None        -> None

let compress_offset tbl ?(max_window = default_max_window) buf off =
  let key = key buf off in
  let candidates = find tbl key in
  let rec aux acc = function
    | []   -> acc
    | i::t ->
      if i >= off || off - i > max_window then
        acc
      else match longuest_substring buf i off with
        | None     -> aux acc t
        | Some len -> aux (max_insert acc (Some (i, len))) t
  in
  match aux None candidates with
  | None          -> None
  | Some (i, len) -> Some (off - i, len)

let size_of = function
  | None        -> 1
  | Some (_, l) -> l

let compress ?max_window buf =
  let res = ref [] in
  let off = ref 0 in
  let len = Cstruct.len buf in
  let tbl = Hashtbl.create 1024 in
  let last = ref 0 in
  let flush_last () =
    if !last <> 0 then (
      let s = Cstruct.sub buf (!off - !last) !last in
      last := 0;
      res := Buffer s :: !res;
    )
  in
  while !off < len do
    match compress_offset tbl ?max_window buf !off with
    | None ->
      add tbl (key buf !off) !off;
      incr last;
      incr off
    | Some (start, len) ->
      for i = !off to !off + len - 1 do add tbl (key buf i) i  done;
      flush_last ();
      res := Insert (start, len) :: !res;
      off := !off + len
  done;
  flush_last ();
  List.rev !res

let compress_as_cstructs ?max_window buf =
  let x = compress ?max_window buf in
  let off = ref 0 in
  List.map (function
      | Buffer b           ->
        off := !off + Cstruct.len b;
        b
      | Insert (diff, len) ->
        off := !off + len;
        Cstruct.sub buf (!off - diff) len
    ) x

let size_of_elt = function
  | Buffer b      -> Cstruct.len b
  | Insert (_, l) -> l

let decompress t =
  let rec length acc = function
    | []   -> acc
    | h::t -> length (size_of_elt h + acc) t
  in
  let buf = Cstruct.create (length 0 t) in
  let rec fill off = function
    | []   -> ()
    | h::t ->
      let () = match h with
        | Buffer b           -> Cstruct.blit b 0 buf off (Cstruct.len b)
        | Insert (diff, len) -> Cstruct.blit buf (off - diff) buf off len
      in
      fill (off + size_of_elt h) t
  in
  fill 0 t;
  buf
