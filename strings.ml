(* ferret string prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * strings.ml
 *)

open Cell
open Interp

(* convert a cell to a readable object *)
let prim_mold st xs =
  let (x,xs') = reduce1 st xs in
  Str (mold x),xs'

(* convert a cell to a pretty string *)
let prim_form st xs =
  let (x,xs') = reduce1 st xs in
  match x with
      Str s -> Str s,xs'
    | _ -> Str (mold x),xs'

(* append one string to another *)
let prim_strcat st xs =
  let (a,xs') = coerce string_of_cell (reduce1 st xs) in
  let (b,xs') = coerce string_of_cell (reduce1 st xs') in
  Str (a ^ b),xs'

(* append one string to another with a space between *)
let prim_strcat_space st xs =
  let (a,xs') = coerce string_of_cell (reduce1 st xs) in
  let (b,xs') = coerce string_of_cell (reduce1 st xs') in
  Str (String.concat " " [a;b]),xs'

(* convert a string to uppercase *)
let prim_uppercase st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  Str (String.uppercase s),xs'

(* convert a string to lowercase *)
let prim_lowercase st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  Str (String.lowercase s),xs'

(* get a substring *)
let prim_sub st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  let (from,xs') = coerce int_of_cell (reduce1 st xs') in
  let (len,xs') = coerce int_of_cell (reduce1 st xs') in
  Str (String.sub s from len),xs'
 
