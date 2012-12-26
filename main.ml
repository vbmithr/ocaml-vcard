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

exception Scanning_error of Lexing.position
exception Syntax_error of Lexing.position

let parse menhir_parser lexbuf =
  let position = ref
    Lexing.({ pos_fname = Sys.argv.(1); pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }) in
  let lexer () =
    let ante_position = !position in
    let nlines, token = Lexer.scanner 1 lexbuf in
    let () = position := Lexing.({!position with pos_lnum = !position.pos_lnum + nlines;}) in
    let post_position = !position
    in (token, ante_position, post_position) in
  let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser
  in try
       revised_parser lexer
    with
      | Ulexing.Error -> raise (Scanning_error !position)
      | Parser.Error  -> raise (Syntax_error !position)

let file = ref ""
let args = []
let usage = "Usage: ./main.native <options> [fichier] (stdin par default)"

let () =
  Arg.parse args (fun s -> file := s) usage;
  let ch = if !file = "" then stdin else open_in !file in
  let lexbuf = Ulexing.from_utf8_channel ch in
  try
    let cs = parse Parser.vcards lexbuf in
    List.iter (Print.print_vcard stdout) cs
  with
    | Syntax_error p -> Lexing.(Printf.eprintf "Syntax error at line %d, exiting.\n" p.pos_lnum)
