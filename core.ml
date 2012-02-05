(* ferret core prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * core.ml
 *)

open Cell
open Interp

exception Fail of Cell.t

(* create a closure *)
let prim_fn st xs =
  let (args,xs') = coerce list_of_cell (reduce1 st xs) in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  Proc (Closure (st.locals,List.map sym_of_cell args,block)),xs'

(* create a temporary lexical scope *)
let prim_with st xs =
  let (vars,xs') = coerce list_of_cell (reduce1 st xs) in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  let bind env p = ((sym_of_cell p).Atom.i,ref Undef)::env in
  let frame = List.fold_left bind st.locals vars in
  interp { st with locals=frame } block,xs'

(* return early from a function *)
let prim_return st xs =
  let (x,_) = reduce1 st xs in
  raise (Return x)

(* raise a failure assertion *)
let prim_fail st xs =
  let (x,_) = reduce1 st xs in
  raise (Fail x)

(* conditional branch *)
let prim_if st xs =
  let (flag,xs') = coerce bool_of_cell (reduce1 st xs) in
  let (ts,xs') = coerce list_of_cell (reduce1 st xs') in
  let (es,xs') = coerce list_of_cell (reduce1 st xs') in
  interp st (if flag then ts else es),xs'

(* simple while loop *)
let prim_while f st xs =
  let (test,xs') = coerce list_of_cell (reduce1 st xs) in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  while bool_of_cell (interp st test) = f do
    ignore (interp st block)
  done;
  Undef,xs'

(* inverse of a boolean *)
let prim_not st xs =
  let (b,xs') = coerce bool_of_cell (reduce1 st xs) in
  Bool (not b),xs'

(* logical and *)
let prim_and st xs =
  let (lval,xs') = coerce bool_of_cell (reduce1 st xs) in
  let (rval,xs') = coerce bool_of_cell (reduce1 st xs') in
  Bool (lval && rval),xs'

(* logical or *)
let prim_or st xs =
  let (lval,xs') = coerce bool_of_cell (reduce1 st xs) in
  let (rval,xs') = coerce bool_of_cell (reduce1 st xs') in
  Bool (lval || rval),xs'

(* apply a function with arguments in a block *)
let prim_apply st xs =
  let (f,xs') = coerce proc_of_cell (reduce1 st xs) in
  let (args,xs') = coerce list_of_cell (reduce1 st xs') in
  let (x,_) = call st args f in
  x,xs' 

(* apply a block *)
let prim_do st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  interp st block,xs'

(* reduce all the values in a block *)
let prim_reduce st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  let rec reduce_ = function
    | [] -> []
    | xs -> let (x,xs') = reduce1 st xs in x::reduce_ xs'
  in
  Block (reduce_ block),xs'

(* load a string, parsing it and returning a block *)
let prim_load st xs =
  let (s,xs') = coerce string_of_cell (reduce1 st xs) in
  match Reader.tokenize s with
      Some (tokens,_) -> Block tokens,xs'
    | None -> raise Reader.Parse_error

(* execute a block forever *)
let prim_forever st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  while true do
    ignore (interp st block)
  done;
  Undef,xs'

(* functional for loop iterator *)
let prim_for st xs =
  let (i,xs') = coerce sym_of_cell (pop1 xs) in
  let (a,xs') = coerce sym_of_cell (pop1 xs') in
  let (count,xs') = coerce int_of_cell (reduce1 st xs') in
  let (initial,xs') = reduce1 st xs' in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  let i' = ref Undef in
  let a' = ref initial in
  let st' = { st with locals=(i.Atom.i,i')::(a.Atom.i,a')::st.locals } in 
  for n = 0 to count - 1 do
    i' := Num (Int n);
    a' := interp st' block
  done;
  !a',xs'

(* functional list iterator *)
let prim_foreach st xs =
  let (i,xs') = coerce sym_of_cell (pop1 xs) in
  let (a,xs') = coerce sym_of_cell (pop1 xs') in
  let (list,xs') = coerce list_of_cell (reduce1 st xs') in
  let (initial,xs') = reduce1 st xs' in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  let i' = ref Undef in
  let a' = ref initial in
  let st' = { st with locals=(i.Atom.i,i')::(a.Atom.i,a')::st.locals } in 
  let f x = 
    i' := x;
    a' := interp st' block
  in
  List.iter f list;
  !a',xs'

