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
  let x = mvar.contents in
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

(* read the current value of an mvar *)
let read mvar =
  Mutex.lock mvar.lock;

  (* wait until there is something to read... *)
  while mvar.contents = None do
    Condition.wait mvar.take_cond mvar.lock
  done;

  (* get the current contents - don't empty the mailbox *)
  let x = mvar.contents in

  (* done *)
  Mutex.unlock mvar.lock;

  (* the read value *)
  x

