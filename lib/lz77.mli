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

(** LZ77 compression algorithm. *)

(** {1 Compressed elements} *)

(** The type of compressed elements. *)
type elt =
  | Buffer of Cstruct.t (** Raw buffer *)
  | Insert of int * int (** negative offset x lenght *)

val compare_elt: elt -> elt -> int
(** Compare compressed elements. *)

val pp_elt: Format.formatter -> elt -> unit
(** Pretty-print a compressed element. *)

(** {2 Compression} *)

type t = elt list
(** The type for compression sequences. *)

val compare: t -> t -> int
(** Compare compression sequences. *)

val pp: Format.formatter -> t -> unit
(** Pretty-print a compression sequence. *)

val compress: ?max_window:int -> Cstruct.t -> elt list
(** Compress a buffer. *)

val decompress: elt list -> Cstruct.t
(** Decompress a sequence of compressed elements. *)

val compress_as_cstructs: ?max_window:int -> Cstruct.t -> Cstruct.t list
(** Compress a buffer but returns a list of (duplicated) buffers. *)
