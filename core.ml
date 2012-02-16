(* ferret core prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * core.ml
 *)

open Cell
open Interp

exception ArityMismatch of Cell.t list
exception Fail of Cell.t

(* create a closure *)
let prim_fn st xs =
  let (args,xs') = coerce list_of_cell (reduce1 st xs) in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  Proc (Closure (st.stack,List.map sym_of_cell args,block)),xs'

(* create a variable from a word *)
let prim_var st xs =
  let (s,xs') = coerce sym_of_cell (reduce1 st xs) in
  Word (Word.Var s),xs'

(* create a new lexical scope *)
let prim_let st xs =
  let (vars,xs') = coerce list_of_cell (reduce1 st xs) in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  let rec bind env xs = 
    if xs = []
    then env
    else begin
      let (var,xs') = coerce var_of_cell (pop1 xs) in
      let (x,xs') = reduce1 st xs' in
      bind ((var.Atom.i,ref x)::env) xs'
    end
  in
  interp { st with stack=bind st.stack vars } block,xs'

(* return early from a function *)
let prim_return st xs =
  let (x,_) = reduce1 st xs in
  raise (Return x)

(* raise a failure assertion *)
let prim_fail st xs =
  let (x,_) = reduce1 st xs in
  raise (Fail x)

(* clone a new prototype and bind values to it *)
let prim_make st xs =
  let (proto,xs') = coerce obj_of_cell (reduce1 st xs) in
  let (vars,xs') = coerce list_of_cell (reduce1 st xs') in
  let rec bind map xs =
    if xs = []
    then map
    else begin
      let (var,xs') = coerce var_of_cell (pop1 xs) in
      let (x,xs') = reduce1 st xs' in
      bind (Atom.AtomMap.add var x map) xs'
    end
  in
  Obj (ref (bind !proto vars)),xs'

(* extract an binding from an object *)
let prim_get st xs =
  let (obj,xs') = coerce obj_of_cell (reduce1 st xs) in
  let (slot,xs') = coerce sym_of_cell (reduce1 st xs') in
  try
    Atom.AtomMap.find slot !obj,xs'
  with Not_found -> Undef,xs'

(* redefine a binding for an object *)
let prim_set st xs =
  let (obj,xs') = coerce obj_of_cell (reduce1 st xs) in
  let (slot,xs') = coerce sym_of_cell (reduce1 st xs') in
  let (x,xs') = reduce1 st xs' in
  obj := Atom.AtomMap.add slot x !obj;
  x,xs'

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
  match call st args f with
      (x,[]) -> x,xs'
    | (_,xs) -> raise (ArityMismatch xs)

(* apply a block *)
let prim_do st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  interp st block,xs'

(* try and apply a block *)
let prim_try st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  let (err,xs') = coerce list_of_cell (reduce1 st xs') in
  try
    interp st block,xs'
  with _ -> interp st err,xs'

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
  let (count,xs') = coerce int_of_cell (reduce1 st xs') in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  let i' = ref Undef in
  let x' = ref Undef in
  let st' = { st with stack=(i.Atom.i,i')::st.stack } in 
  for n = 0 to count - 1 do
    i' := Num (Int n);
    x' := interp st' block
  done;
  !x',xs'

(* functional list iterator *)
let prim_foreach st xs =
  let (i,xs') = coerce sym_of_cell (pop1 xs) in
  let (list,xs') = coerce list_of_cell (reduce1 st xs') in
  let (block,xs') = coerce list_of_cell (reduce1 st xs') in
  let i' = ref Undef in
  let x' = ref Undef in
  let st' = { st with stack=(i.Atom.i,i')::st.stack } in 
  let f x = 
    i' := x;
    x' := interp st' block
  in
  List.iter f list;
  !x',xs'

