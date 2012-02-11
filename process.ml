(* ferret process prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * process.ml
 *)

open Cell
open Interp

exception Kill

(* kill signal *)
let kill_lock = Mutex.create ()
let kill_sig = ref None

(* on every context switch, see if the process is being killed *)
let install_kill_signal () =
  let force_interrupt _ =
    let self = Thread.id (Thread.self ()) in
    if Some self = !(kill_sig) then raise Kill
  in
  let platform_signal =
    match Sys.os_type with
        "Win32" -> Sys.sigterm
      | _ -> Sys.sigvtalrm
  in
  ignore (Sys.signal platform_signal (Sys.Signal_handle force_interrupt))

(* spawn a new process and start it running *)
let prim_spawn st xs =
  let (block,xs') = coerce list_of_cell (reduce1 st xs) in
  fork_thread st block,xs'

(* get the current process's pid *)
let prim_self st xs =
  Pid (Thread.self (),st.pinfo),xs

(* terminate a process *)
let prim_kill st xs =
  let ((t,pinfo),xs') = coerce thread_of_cell (reduce1 st xs) in
  Mutex.lock kill_lock;
  kill_sig := Some (Thread.id t);
  ignore (Thread.join t);
  Mutex.unlock kill_lock;
  Undef,xs'

(* return the current status of a thread *)
let prim_status st xs =
  let ((_,pinfo),xs') = coerce thread_of_cell (reduce1 st xs) in
  if Mvar.is_empty pinfo.status
  then Word (Word.Sym (Atom.intern "running")),xs'
  else begin
    match Mvar.read pinfo.status with
        Completed _ -> Word (Word.Sym (Atom.intern "completed")),xs'
      | Terminated _ -> Word (Word.Sym (Atom.intern "terminated")),xs'
  end

(* wait for a process to complete *)
let prim_join st xs =
  let ((t,pinfo),xs') = coerce thread_of_cell (reduce1 st xs) in
  Thread.join t;
  match Mvar.read pinfo.status with
      Completed x -> x,xs'
    | Terminated e -> raise e

(* sleep this thread for a period of time *)
let prim_sleep st xs =
  let (sec,xs') = coerce float_of_cell (reduce1 st xs) in
  Thread.delay sec;
  Undef,xs'

(* let other threads execute *)
let prim_yield st xs =
  Thread.yield ();
  Undef,xs

(* send a message to another process *)
let prim_send st xs =
  let ((_,pinfo),xs') = coerce thread_of_cell (reduce1 st xs) in
  let (msg,xs') = reduce1 st xs' in
  Mutex.lock pinfo.lock;
  Queue.add msg pinfo.mailbox;
  Mutex.unlock pinfo.lock;
  Condition.signal pinfo.full;
  msg,xs'

(* receive a message from another process *)
let prim_receive st xs =
  Mutex.lock st.pinfo.lock;
  try
    Condition.wait st.pinfo.full st.pinfo.lock;
    let x = Queue.take st.pinfo.mailbox in
    if not (Queue.is_empty st.pinfo.mailbox)
    then begin
      Condition.signal st.pinfo.full
    end;
    Mutex.unlock st.pinfo.lock;
    x,xs
  with e -> (Mutex.unlock st.pinfo.lock; raise e)

