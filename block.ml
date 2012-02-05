(* ferret block prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * block.ml
 *)

open Cell
open Interp

(* insert a cell at the front of a block *)
let prim_cons st xs =
  let (lval,xs') = pop1 xs in
  let (rval,xs') = coerce list_of_cell (pop1 xs') in
  Block (lval::rval),xs'

(* check to see if a block is empty *)
let prim_null st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  Bool (block = []),xs'

(* fetch the head element of a list *)
let prim_head st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  match block with
      [] -> Undef,xs'
    | x::_ -> x,xs'

(* fetch the tail element of a list *)
let prim_tail st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  match block with
      [] -> Undef,xs'
    | _::xs -> (Block xs),xs'

(* get the length of a block *)
let prim_len st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  Num (Int (List.length block)),xs'

(* reverse a block *)
let prim_rev st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  Block (List.rev block),xs'

(* create a simple pair block *)
let prim_pair st xs =
  let (f,xs') = reduce1 st xs in
  let (s,xs') = reduce1 st xs' in
  Block [f;s],xs'

(* get the first element of a pair block *)
let prim_fst st xs =
  let (pair,xs') = coerce pair_of_cell (reduce1 st xs) in
  fst pair,xs'

(* get the second element of a pair block *)
let prim_snd st xs =
  let (pair,xs') = coerce pair_of_cell (reduce1 st xs) in
  snd pair,xs'

