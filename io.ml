(* ferret io prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * io.ml
 *)

open Cell
open Interp

(* the standard file handles *)
let prim_stdout st xs = Port_out (stdout,"stdout"),xs
let prim_stdin st xs = Port_in (stdin,"stdin"),xs
let prim_stderr st xs = Port_out (stderr,"stderr"),xs

(* open a file for reading *)
let prim_open st xs = 
  let (f,xs') = coerce string_of_cell (reduce1 st xs) in
  Port_in (open_in f,f),xs'

(* close an open port *)
let prim_close st xs =
  let (x,xs') = reduce1 st xs in
  begin
    match x with
        Port_in (chan,_) -> close_in chan
      | Port_out (chan,_) -> close_out chan
      | _ -> raise (Not_a_port x)
  end;
  Undef,xs'

(* flush an output port *)
let prim_flush st xs =
  let (chan,xs') = coerce out_chan_of_cell (reduce1 st xs) in
  flush chan;
  Undef,xs'

(* write a string to an output port *)
let prim_write st xs =
  let (chan,xs') = coerce out_chan_of_cell (reduce1 st xs) in
  let (s,xs') = coerce string_of_cell (reduce1 st xs') in
  output_string chan s;
  Undef,xs'

(* write a string with a newline to an output port *)
let prim_write_line st xs =
  let (chan,xs') = coerce out_chan_of_cell (reduce1 st xs) in
  let (s,xs') = coerce string_of_cell (reduce1 st xs') in
  output_string chan s;
  output_char chan '\n';
  Undef,xs'

(* read the entire contents of an input port *)
let prim_read st xs =
  let (chan,xs') = coerce in_chan_of_cell (reduce1 st xs) in
  let buf = Buffer.create 1024 in
  begin 
    try
      while true do
        Buffer.add_char buf (input_char chan)
      done
    with End_of_file -> ()
  end;
  Str (Buffer.contents buf),xs'

(* read a singe line from an input port *)
let prim_read_line st xs =
  let (chan,xs') = coerce in_chan_of_cell (reduce1 st xs) in
  Str (input_line chan),xs'

