(* canopy core
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * canopy.ml
 *)

(* dynamically scoped and lexically scoped environments *)
type dynamic_env = (int,Cell.t) Hashtbl.t
type local_env = (int * Cell.t ref) list

(* corouting result *)
type process_result =
    Completed of Cell.t
  | Terminated of exn

(* coroutine process info *)
type process_info =
    { mailbox : Cell.t Queue.t
    ; status  : process_result option ref
    }

(* thread state *)
type st =
    { env    : dynamic_env
    ; locals : local_env
    ; pinfo  : process_info
    }
