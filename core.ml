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
exception Empty_list
exception No_iterator

(* kill signal *)
let kill_lock = Mutex.create ()
let kill_sig = ref None

(* list the current modules *)
let prim_modules st =
  let xs = List.map (fun (m,_) -> Atom m) st.env in
  { st with stack=List xs::st.stack }

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

(* attempt to apply a block, if it fails, catch the exception *)
let prim_try st =
  match st.stack with
      [] -> raise Stack_underflow
    | b::xs -> 
      try 
        let st' = apply { st with stack=xs } b in
        { st' with stack=Bool true::st'.stack }
      with _ -> { st with stack=Bool false::xs }
  
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

(* push the current iterator *)
let prim_i st =
  match st.i with
      None -> raise No_iterator
    | Some x -> { st with stack=x::st.stack }

(* insert a new element at the head of a list *)
let prim_cons st =
  match st.stack with
      k::ks::xs -> { st with stack=List (k::list_of_cell ks)::xs }
    | _ -> raise Stack_underflow

(* split a list into the head and tail *)
let prim_uncons st =
  let (xs,st') = coerce list_of_cell (pop st) in
  match xs with
      [] -> raise Empty_list
    | (k::ks) -> { st' with stack=k::List ks::st'.stack }

(* true if a list is empty *)
let prim_null st =
  match st.stack with
      x::xs -> { st with stack=Bool ([] = list_of_cell x)::xs }
    | _ -> raise Stack_underflow

(* length of a list *)
let prim_len st =
  match st.stack with
      x::xs -> { st with stack=Num (Int (List.length (list_of_cell x)))::xs }
    | _ -> raise Stack_underflow

(* reverse a list *)
let prim_rev st =
  match st.stack with
      x::xs -> { st with stack=List (List.rev (list_of_cell x))::xs }
    | _ -> raise Stack_underflow

(* get the head of a list *)
let prim_hd st =
  let (xs,st') = coerce list_of_cell (pop st) in
  match xs with
      x::_ -> { st' with stack=x::st'.stack }
    | _ -> raise Empty_list

(* get the tail of a list *)
let prim_tl st =
  let (xs,st') = coerce list_of_cell (pop st) in
  match xs with
      _::x -> { st' with stack=List x::st'.stack }
    | _ -> raise Empty_list





(* spawn a new process *)
let prim_spawn st =
  let (b,st') = coerce block_of_cell (pop st) in
  { st' with stack=(fork_thread (spawn_thread st') b)::st'.stack }

(* push the current process id *)
let prim_self st =
  { st with stack=Pid (Thread.self (),st.pinfo)::st.stack }

(* kill a process *)
let prim_kill st =
  let ((t,pinfo),st') = coerce thread_of_cell (pop st) in
  Mutex.lock kill_lock;
  kill_sig := Some (Thread.id t);
  ignore (Thread.join t);
  Mutex.unlock kill_lock;
  st'

(* return the current status of a thread *)
let prim_status st =
  let ((_,pinfo),st') = coerce thread_of_cell (pop st) in
  let running = Atom.intern "Running" in
  let completed = Atom.intern "Completed" in
  let terminated = Atom.intern "Terminated" in
  if Mvar.is_empty pinfo.status
  then { st' with stack=Atom running::st'.stack }
  else begin
    match Mvar.read pinfo.status with
        Completed _ -> { st' with stack=Atom completed::st'.stack }
      | Terminated _ -> { st' with stack=Atom terminated::st'.stack }
  end

(* wait for a process to complete *)
let prim_join st =
  let ((t,pinfo),st') = coerce thread_of_cell (pop st) in
  Thread.join t;
  match Mvar.read pinfo.status with
      Completed st -> { st' with stack=Bool true::List st.stack::st'.stack }
    | Terminated e -> { st' with stack=Bool false::st'.stack }

(* sleep this thread for a period of time *)
let prim_sleep st =
  let (sec,st') = coerce float_of_cell (pop st) in
  Thread.delay sec;
  st'

(* let other threads execute *)
let prim_yield st =
  Thread.yield ();
  st

(* send a message to another process *)
let prim_send st =
  let ((_,pinfo),st') = coerce thread_of_cell (pop st) in
  let (msg,st') = pop st' in
  Mutex.lock pinfo.lock;
  Queue.add msg pinfo.mb;
  Mutex.unlock pinfo.lock;
  st'

(* receive a message from another process *)
let rec prim_receive st =
  Mutex.lock st.pinfo.lock;
  while Queue.is_empty st.pinfo.mb do
    Mutex.unlock st.pinfo.lock;
    Thread.delay 0.1;
    Mutex.lock st.pinfo.lock
  done;
  let x = Queue.take st.pinfo.mb in
  Mutex.unlock st.pinfo.lock;
  { st with stack=x::st.stack }

