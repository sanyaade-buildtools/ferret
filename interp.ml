(* ferret interpreter
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * interp.ml
 *)

open Cell
open Compiler

exception Return of Cell.st
exception Stack_underflow

(* parse and interpret a string *)
let rec eval st src = 
  let xs,st' = compile st src in interp st' xs

(* interpret a list of tokens *)
and interp st = List.fold_left execute st

(* pop a single value from the stack *)
and pop st = 
  match st.stack with
      x::xs -> x,{ st with stack=xs }
    | [] -> raise Stack_underflow

(* apply a block *)
and apply st x =
  let ps,xs = block_of_cell x in
  { interp { st with locals=ps } xs with locals=st.locals }

(* execute a token *)
and execute st = function
  | Word (_,word) -> do_word st word
  | Local (atom) -> do_local st atom
  | With (ps,xs) -> do_with st ps xs
  | If (ts,es) -> do_if st ts es
  | While (ts,xs) -> do_while st ts xs
  | Until (xs) -> do_until st xs
  | Loop (xs) -> do_loop st xs
  | For (xs) -> do_for st xs
  | Each (xs) -> do_each st xs
  | Lit (x) -> do_push st x
  | Recurse -> st
  | Exit -> raise (Return st)

(* execute a word *)
and do_word st word = 
  match word.def with
    | Colon xs -> do_colon st xs
    | Prim p -> p st

(* call a colon definition, catching returns *)
and do_colon st xs =
  try interp st xs with Return st' -> st' | e -> raise e

(* push a local *)
and do_local st atom =
  { st with stack=List.assq atom.Atom.i st.locals::st.stack }

(* create a new lexical scope *)
and do_with st ps xs =
  let bind st p = 
    match st.stack with
        x::xs -> { st with locals=(p.Atom.i,x)::st.locals; stack=xs }
      | [] -> raise Stack_underflow
  in
  interp (List.fold_left bind st ps) xs

(* conditional branch *)
and do_if st ts es =
  let (flag,st') = coerce bool_of_cell (pop st) in
  interp st' (if flag then ts else es)

(* while loop *)
and do_while st ts xs =
  let rec loop st = 
    let (flag,st') = coerce bool_of_cell (pop (interp st ts)) in
    if flag then loop (interp st' xs) else st'
  in
  loop st

(* until loop *)
and do_until st xs =
  let rec loop st =
    let (flag,st') = coerce bool_of_cell (pop (interp st xs)) in
    if flag then loop st' else st'
  in
  loop st

(* infinite loop *)
and do_loop st xs = do_loop (interp st xs) xs

(* for loop *)
and do_for st xs =
  let rec loop st = function
    | 0 -> st
    | i -> loop (interp { st with i=Some (Num (Int (i-1))) } xs) (i-1)
  in
  let (i,st') = coerce int_of_cell (pop st) in
  loop st' i

(* each loop *)
and do_each st xs =
  let rec loop st = function
    | [] -> st
    | c::cs -> loop (interp { st with i=Some c } xs) cs
  in
  let (cs,st') = coerce list_of_cell (pop st) in
  loop st' cs

(* push literal *)
and do_push st = function
  | Block (_,xs) -> { st with stack=Block (st.locals,xs)::st.stack }
  | x -> { st with stack=x::st.stack }

(* count a reduction and maybe yield *)
let yield st = 
  incr st.reducs;
  if !(st.reducs) land 0x80 = 0x80
  then begin
    Thread.yield ()
  end

(* run a block until completed *)
let run_thread st block =
  try
    let st' = interp st block in Mvar.put st.pinfo.status (Completed st')
  with e -> Mvar.put st.pinfo.status (Terminated e)

(* start a spawned thread running in a child process *)
let fork_thread st block =
  let st' = spawn_thread st in
  Pid ((Thread.create (run_thread st') block),st'.pinfo)

