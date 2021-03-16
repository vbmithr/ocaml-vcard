(*
 * Copyright (c) 2012 Vincent Bernardoff <vb@luminar.eu.org>
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
 *
 *)

open Ast

let print_param ch (pid, pvs) =
  match pvs with
    | h::t ->
      Printf.fprintf ch ";%s=%s" pid h;
      List.iter (fun pv -> Printf.fprintf ch ",%s" pv) t
    | [] -> failwith "print_param"

let print_content_line ch { name; params; value } =
  Printf.fprintf ch "%s" name;
  List.iter (print_param ch) params;
  Printf.fprintf ch ":%s\r\n" value

let print_vcard ch vcard =
  Printf.fprintf ch "BEGIN:VCARD\r\n";
  List.iter (print_content_line ch) vcard;
  Printf.fprintf ch "END:VCARD\r\n"
