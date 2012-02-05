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
    | Char c -> Str (String.make 1 c),xs'
    | _ -> Str (mold x),xs'

(* convert a character to an integer *)
let prim_ord st xs =
  let (c,xs') = coerce char_of_cell (reduce1 st xs) in
  Num (Int (Char.code c)),xs'

(* convert an integer to a character *)
let prim_chr st xs =
  let (i,xs') = coerce int_of_cell (reduce1 st xs) in
  Char (Char.chr i),xs'

(* convert a character to uppercase *)
let prim_toupper st xs =
  let (c,xs') = coerce char_of_cell (reduce1 st xs) in
  Char (Char.uppercase c),xs'

(* convert a character to lowercase *)
let prim_tolower st xs =
  let (c,xs') = coerce char_of_cell (reduce1 st xs) in
  Char (Char.lowercase c),xs'

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

(* find the index of a character in a string *)
let prim_scan st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  let (c,xs') = coerce char_of_cell (reduce1 st xs') in
  try
    Num (Int (String.index s c)),xs'
  with Not_found -> Undef,xs'

(* find the index of a character in a string starting from the end *)
let prim_rscan st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  let (c,xs') = coerce char_of_cell (reduce1 st xs') in
  try
    Num (Int (String.rindex s c)),xs'
  with Not_found -> Undef,xs'

(* convert a string to uppercase *)
let prim_uppercase st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  Str (String.uppercase s),xs'

(* convert a string to lowercase *)
let prim_lowercase st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  Str (String.lowercase s),xs'

