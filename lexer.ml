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

open Ulexing
open Parser

(* RFC 5234 *)

let regexp alpha  = ['A'-'Z''a'-'z']
let regexp bit    = ["01"]
let regexp char   = [0x1-0x7f]
let regexp cr     = '\r'
let regexp crlf   = "\r\n"
let regexp ctl    = [0-0x1f 0x7f]
let regexp digit  = ['0'-'9']
let regexp dquote = '"'
let regexp hexdig = ['0'-'9''A'-'F''a'-'f']
let regexp htab   = '\t'
let regexp lf     = '\n'
let regexp octet  = [0-0xff]
let regexp sp     = 0x20
let regexp vchar = [0x21-0x7e]
let regexp wsp    = [' ''\t']

(* RFC 6350 *)
let regexp nonascii = [0x80-0x10ffff]
let regexp qsafechar = (wsp | '!' | [0x23-0x7e] | nonascii)
let regexp safechar = (wsp | '!' | [0x23-0x39] | [0x3c-0x7e] | nonascii)
let regexp valuechar = (wsp | vchar | nonascii)

let regexp alphadigit = (alpha | digit | '-')

let rec value_scanner nlines buf = lexer
  | crlf wsp -> value_scanner (succ nlines) buf lexbuf
  | crlf -> (succ nlines), Buffer.contents buf
  | valuechar+ ->
    Buffer.add_string buf (utf8_lexeme lexbuf);
    value_scanner nlines buf lexbuf

let rec main_scanner nlines = lexer
  | crlf wsp  -> main_scanner (succ nlines) lexbuf
  | crlf      -> main_scanner (succ nlines) lexbuf
  | ','       -> nlines, COMMA
  | ';'       -> nlines, SEMI
  | ':'       ->
    let nlines, content = value_scanner nlines (Buffer.create 100) lexbuf in
    nlines, VALUE(content)
  | '='       -> nlines, EQUAL
  | dquote    -> nlines, DQUOTE

  | [^"\n\r\t ,;:=\""]+ -> nlines, ID(utf8_lexeme lexbuf)
  | eof       -> nlines, EOF
