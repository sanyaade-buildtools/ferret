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
  interp { st with locals=bind st.locals vars } block,xs'

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
  let (s,xs') = coerce sym_of_cell (reduce1 st xs') in
  try
    Atom.AtomMap.find s !obj,xs'
  with Not_found -> Undef,xs'

(* redefine a binding for an object *)
let prim_set st xs =
  let (obj,xs') = coerce obj_of_cell (reduce1 st xs) in
  let (s,xs') = coerce sym_of_cell (reduce1 st xs') in
  let (x,xs') = reduce1 st xs' in
  begin
    if x = Undef
    then obj := Atom.AtomMap.remove s !obj
    else obj := Atom.AtomMap.add s x !obj
  end;
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

