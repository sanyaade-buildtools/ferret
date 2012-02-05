(* ferret interpreter
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * interp.ml
 *)

open Cell

exception Missing_arg
exception Unbound_symbol of string
exception Return of Cell.t

(* import a file *)
let rec import st file =
    let chan = open_in file in
    let len = in_channel_length chan in
    let source = String.create len in
    really_input chan source 0 len;
    close_in chan;
    ignore (eval st source)

(* parse and interpret a string *)
and eval st src =
  match Reader.tokenize src with
      Some (tokens,_) -> interp st tokens
    | None -> raise Reader.Parse_error

(* interpret an entire frame, return the last value reduced *)
and interp st = function
  | [] -> Undef
  | xs ->
    match reduce1 st xs with
        (x,[]) -> x
      | (_,xs') -> interp st xs'

(* pop a value from the frame, don't reduce it *)
and pop1 = function
  | [] -> raise Missing_arg
  | x::xs -> x,xs

(* reduce a single value from the frame *)
and reduce1 st = function
  | (Word (Word.Var f)::xs) -> var st f xs
  | (x::Word (Word.Binary (_,f))::xs) -> binary st f x xs
  | (xs) -> primary st xs

(* perform a primary operation *)
and primary st = function
  | (Word f::xs) -> word st xs f
  | (Expr block::xs) -> expr st block xs
  | (Quote x::xs) -> x,xs
  | (x::xs) -> x,xs
  | (_) -> raise Missing_arg

(* perform a binary operation *)
and binary st f x xs =
  let (lval,_) = primary st [x] in
  let (rval,xs') = primary st xs in
  let (x',_) = apply st [lval;rval] (lookup st f) in
  reduce1 st (x'::xs')

(* evaluate a word *)
and word st xs = function
  | Word.Var f -> var st f xs
  | Word.Getter f -> lookup st f,xs
  | f -> apply st xs (lookup st (Word.atom f))

(* apply the frame to a procedure *)
and apply st xs = function
  | Proc proc -> call st xs proc
  | x -> x,xs

(* call a procedure with a list of arguments *)
and call st xs = function
  | Closure (env,ps,block) -> funcall st env ps block xs
  | Prim (_,p) -> p st xs

(* call a closure, binding new locals, and executing *)
and funcall st env ps block xs =
  let (env',xs') = frame st env xs ps in
  try
    let x = interp { st with locals=env' } block in x,xs'
  with
      Return x -> x,xs'
    | e -> raise e

(* create a new local frame from a list of bindings *)  
and frame st env xs = 
  let bind (env,xs) p =
    let (x,xs') = reduce1 st xs in 
    if p.Atom.name.[0] = '_'
    then env,xs'
    else (p.Atom.i,ref x)::env,xs'
  in
  List.fold_left bind (env,xs)

(* create a frame with new bindings, undefined *)
and empty_frame st =
  let bind env p = (p.Atom.i,ref Undef)::env in
  List.fold_left bind st.locals

(* bind a value to a variable *)
and var st f xs =
  let ((x,xs') as r) = reduce1 st xs in
  begin
    try 
      List.assoc f.Atom.i st.locals := x
    with Not_found -> Hashtbl.replace st.env f.Atom.i x
  end;
  r

(* interpret a block and return back *)
and expr st block xs = interp st block,xs

(* lookup a local or global binding *)
and lookup st f =
  try !(List.assoc f.Atom.i st.locals) with Not_found -> 
  try Hashtbl.find st.env f.Atom.i with Not_found ->
  raise (Unbound_symbol f.Atom.name)

