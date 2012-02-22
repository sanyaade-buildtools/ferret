(* ferret core prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * core.ml
 *)

open Cell
open Interp

exception Abort of string
exception Control_stack_underflow

(* raise a failure assertion *)
let prim_abort st =
  let (s,_) = coerce string_of_cell (pop st) in raise (Abort s)

(* clear the stacks *)
let prim_clear st =
  { st with stack=[]; cs=[] }

(* remove the top stack item *)
let prim_drop st = 
  match st.stack with
      x::xs -> { st with stack=xs }
    | _ -> raise Stack_underflow

(* duplicate the top stack item *)
let prim_dup st =
  match st.stack with
      x::xs -> { st with stack=x::x::xs }
    | _ -> raise Stack_underflow

(* swap the top two stack items *)
let prim_swap st =
  match st.stack with
      a::b::xs -> { st with stack=b::a::xs }
    | _ -> raise Stack_underflow

(* duplicate the second stack item *)
let prim_over st =
  match st.stack with
      a::b::xs -> { st with stack=b::a::b::xs }
    | _ -> raise Stack_underflow

(* push the top of the stack to the control stack *)
let prim_push st =
  match st.stack with
      x::xs -> { st with stack=xs; cs=x::st.cs }
    | _ -> raise Stack_underflow

(* pop the top of the control stack *)
let prim_pop st =
  match st.cs with
      c::cs -> { st with stack=c::st.stack; cs=cs }
    | _ -> raise Control_stack_underflow

(* get the top of the control stack *)
let prim_get st =
  match st.cs with
      c::cs -> { st with stack=c::st.stack }
    | _ -> raise Control_stack_underflow

(* replace the top of the control stack *)
let prim_put st =
  match st.stack,st.cs with
      (x::xs,c::cs) -> { st with stack=xs; cs=x::cs }
    | ([],_) -> raise Stack_underflow
    | (_,[]) -> raise Control_stack_underflow

(* apply a block *)
let prim_apply st =
  match st.stack with
      b::xs -> apply { st with stack=xs } b
    | _ -> raise Stack_underflow

(* push to control stack, apply block, pop control stack *)
let prim_do st =
  match st.stack with
      b::x::xs -> apply { st with stack=xs; cs=x::st.cs } b
    | _ -> raise Stack_underflow

(* lift a block into the control stack and apply it *)
let prim_lift st =
  match st.stack,st.cs with
      (b::xs,c::cs) -> { apply { st with stack=xs; cs=cs } b with cs=st.cs }
    | ([],_) -> raise Stack_underflow
    | (_,[]) -> raise Control_stack_underflow

