(* ferret io prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * io.ml
 *)

open Cell
open Http_client.Convenience
open Interp
open Parsec

exception Invalid_filename of string
exception Invalid_url of string
exception Invalid_email of string

(* parse a filename *)
let parse_file s = File s

(* read the contents of a file *)
let read_file f =
  let chan = open_in_bin f in
  try
    let len = in_channel_length chan in
    let s = String.create len in
    really_input chan s 0 len;
    close_in chan;
    Str s
  with e ->
    close_in chan;
    raise e

(* read the contents of a url *)
let read_url url = Str (http_get (Neturl.string_of_url url))

(* coerce a string to a file *) 
let prim_file st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  Filespec (File s),xs'

let prim_url st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  Filespec (Url (Neturl.parse_url s)),xs'

(* read a line of input from stdin *)
let prim_input st xs =
  Str (read_line ()),xs

(* print a formed object *)
let prim_print f st xs =
  let (x,xs') = reduce1 st xs in
  let s = match x with
      Str s -> s
    | x -> mold x
  in
  f s;
  flush stdout;
  Undef,xs'

(* print a character to stdout *)
let prim_princ c st xs =
  print_char c;
  flush stdout;
  Undef,xs

(* read a string from a source location *)
let prim_read st xs =
  let (spec,xs') = coerce spec_of_cell (reduce1 st xs) in
  let x = 
    match spec with
        File file -> read_file file
      | Url url -> read_url url
  in
  x,xs'

(* load source, parsing it and returning a block *)
let prim_load st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  match Reader.tokenize s with
      Some (tokens,_) -> Block tokens,xs'
    | None -> raise Reader.Parse_error

