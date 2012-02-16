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
  | [] -> begin yield st; Undef end
  | xs ->
    match reduce1 st xs with
        (x,[]) -> x
      | (_,xs') -> interp st xs'

(* count a reduction and maybe yield *)
and yield st = 
  incr st.reductions;
  if !(st.reductions) land 0x80 = 0x80
  then begin
    Thread.yield ()
  end

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
  let (x',_) = apply st [Quote lval;Quote rval] (lookup st f) in
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
  | Closure (env,ps,block) -> begin yield st; funcall st env ps block xs end
  | Prim (_,p) -> begin yield st; p st xs end

(* call a closure, binding new locals, and executing *)
and funcall st env ps block xs =
  let (env',xs') = frame st env xs ps in
  try
    let x = interp { st with stack=env' } block in x,xs'
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
  List.fold_left bind st.stack

(* bind a value to a variable *)
and var st f xs =
  let ((x,xs') as r) = reduce1 st xs in
  begin
    try 
      List.assoc f.Atom.i st.stack := x
    with Not_found -> bind st f x
  end;
  r

(* bind an atom to a value *)
and bind st f x =
  Hashtbl.replace (List.hd st.env) f.Atom.i x

(* interpret a block and return back *)
and expr st block xs = interp st block,xs

(* lookup a local or global binding *)
and lookup st f =
  try !(List.assoc f.Atom.i st.stack) with Not_found ->
  let rec find = function
    | e::es -> (try Hashtbl.find e f.Atom.i with Not_found -> find es)
    | [] -> raise (Unbound_symbol f.Atom.name)
  in
  find st.env  

(* run a block until completed *)
let run_thread st block =
  try
    let x = interp st block in
    Mvar.put st.pinfo.status (Completed x)
  with e -> Mvar.put st.pinfo.status (Terminated e)

(* start a spawned thread running in a child process *)
let fork_thread st block =
  let st' = spawn_thread st in
  Pid ((Thread.create (run_thread st') block),st'.pinfo)

