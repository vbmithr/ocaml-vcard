# ocaml-vcard

This is a simple library for parsing the vCard file format (As defined
in RFC6350). It aims to be flexible enough to handle most common
vCards formats (let’s say, 3.0 and 4.0).

It is implemented with standard OCaml lexing/parsing tools, ocamllex
and menhir.

## Dependencies

* ulex
* menhir

## Building

Type `make`. It will produce a `main.native` executable.

## Usage

`main.native` parse a file containing one or several vcard(s), and
print them on the standard output after parsing.
