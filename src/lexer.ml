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

open Parser

exception Error of int * int

let crlf = [%sedlex.regexp? "\r\n"]
let vchar = [%sedlex.regexp? 0x21 .. 0x7e]
let wsp = [%sedlex.regexp? (' ' | '\t')]
let nonascii = [%sedlex.regexp? 0x80 .. 0x10ffff]
let valuechar = [%sedlex.regexp? (wsp | vchar | nonascii)]

let rec value_scanner nlines buf lexbuf =
  match%sedlex lexbuf with
  | crlf, wsp -> value_scanner (succ nlines) buf lexbuf
  | crlf -> succ nlines, Buffer.contents buf
  | Plus valuechar ->
    Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
    value_scanner nlines buf lexbuf
  | _ -> raise (Error (Sedlexing.lexeme_start lexbuf, Sedlexing.lexeme_end lexbuf))

let rec main_scanner nlines lexbuf =
  match%sedlex lexbuf with
  | crlf, wsp -> main_scanner (succ nlines) lexbuf
  | crlf -> main_scanner (succ nlines) lexbuf
  | ',' -> nlines, COMMA
  | ';' -> nlines, SEMI
  | ':' ->
    let nlines, content = value_scanner nlines (Buffer.create 128) lexbuf in
    nlines, VALUE(content)
  | '=' -> nlines, EQUAL
  | '"' -> nlines, DQUOTE
  | Plus (Sub (any, ('\n' | '\r' | '\t' | ' ' | ',' | ';' | ':' | '=' | '\"' ))) -> nlines, ID (Sedlexing.Utf8.lexeme lexbuf)
  | eof -> nlines, EOF
  | _ -> raise (Error (Sedlexing.lexeme_start lexbuf, Sedlexing.lexeme_end lexbuf))
