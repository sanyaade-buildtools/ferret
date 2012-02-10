(* simple mailbox variable implementation for ocaml
 * 
 * copyright 2012 by jeffrey massung
 * all rights reserved
 *
 * mvar.ml
 *)

(* mailbox varaible *)
type 'a t =
  { mutable contents  : 'a option
  ;         lock      : Mutex.t
  ;         take_cond : Condition.t
  ;         put_cond  : Condition.t
  }

exception No_value

(* helper function *)
let some = function
  | None -> raise No_value
  | Some x -> x

(* create a new, empty mvar *)
let empty () =
  { contents=None
  ; lock=Mutex.create ()
  ; take_cond=Condition.create ()
  ; put_cond=Condition.create ()
  }

(* check to see if a mailbox value is empty *)
let is_empty mvar =
  Mutex.lock mvar.lock;
  let empty = mvar.contents = None in
  Mutex.unlock mvar.lock;
  empty

(* take an mvar, block if it's empty *)
let take mvar = 
  Mutex.lock mvar.lock;

  (* wait until there is something to take... *)
  while mvar.contents = None do
    Condition.wait mvar.take_cond mvar.lock
  done;
  
  (* take it, empty out the contents *)
  let x = some mvar.contents in
  mvar.contents <- None;

  (* signal that it's okay to put something now *)
  Condition.signal mvar.put_cond;
  Mutex.unlock mvar.lock;

  (* taken value *)
  x

(* put a value into an mvar *)
let put mvar x =
  Mutex.lock mvar.lock;

  (* wait until the mailbox is empty... *)
  while mvar.contents <> None do
    Condition.wait mvar.put_cond mvar.lock
  done;

  (* put the new value into the mailbox *)
  mvar.contents <- Some x;

  (* signal that it's okay to take it *)
  Condition.signal mvar.take_cond;
  Mutex.unlock mvar.lock

(* take and immediately put a new value, return old value *)
let swap mvar x =
  let org = take mvar in
  put mvar x;
  org

(* modify the contents of an mvar, return new value *)
let modify mvar f =
  let x' = f (take mvar) in
  put mvar x';
  x'

(* read the current value of an mvar, and put it right back *)
let read mvar =
  modify mvar (fun x -> x)

