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
let regexp bit    = ['0''1']
let regexp char   = ['\x01'-'\x7f']
let regexp cr     = '\r'
let regexp crlf   = "\r\n"
let regexp ctl    = ['\x00'-'\x1f''\x7f']
let regexp digit  = ['0'-'9']
let regexp dquote = "\""
let regexp hexdig = ['0'-'9''A'-'F''a'-'f']
let regexp htab   = '\t'
let regexp lf     = '\n'
let regexp octet  = ['\x00'-'\xff']
let regexp sp     = '\x20'
let regexp vchar = ['\x21'-'\x7e']
let regexp wsp    = [' ''\t']

(* RFC 6350 *)
let regexp nonascii = [^'\x00'-'\xff']
let regexp qsafechar = (wsp | '!' | ['\x23'-'\x7e'] | nonascii)
let regexp safechar = (wsp | '!' | ['\x23'-'\x39'] | ['\x3c'-'\x7e'] | nonascii)
let regexp valuechar = (wsp | vchar | nonascii)

let regexp alphadigit = (alpha | digit | '-')

exception Error of string

let rec scanner nlines = lexer
  | "BEGIN:VCARD" -> nlines, BEGIN_VCARD
  | "END:VCARD" -> nlines, END_VCARD
  | crlf wsp        -> scanner (succ nlines) lexbuf
  | crlf      -> (succ nlines), CRLF
  | ','             -> nlines, COMMA
  | ';'             -> nlines, SEMI
  | ':'             -> nlines,COLON
  | '='             -> nlines, EQUAL
  | dquote -> nlines, DQUOTE

  | safechar -> nlines, ID(utf8_lexeme lexbuf)
  | eof             -> nlines, EOF
