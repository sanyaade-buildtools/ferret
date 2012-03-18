(* ferret entry point
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * main.ml
 *)

open Interp
open Terminal

exception Kill

(* on every context switch, see if the process is being killed *)
let install_kill_signal () =
  let force_interrupt _ =
    let self = Thread.id (Thread.self ()) in
    if Some self = !(Core.kill_sig) then raise Kill
  in
  let platform_signal =
    match Sys.os_type with
        "Win32" -> Sys.sigterm
      | _ -> Sys.sigvtalrm
  in
  ignore (Sys.signal platform_signal (Sys.Signal_handle force_interrupt))

(* read-eval-print loop *)
let rec repl st =
  try
    show_ok ();
    let st = ref st in
    while true do
      try
        st := eval !st (read_line ());
        show_top (!st).Cell.stack;
      with 
          End_of_file -> raise End_of_file
        | e -> show_err e
    done
  with End_of_file -> ()

(* the user module *)
let user = Atom.intern "User",Atom.IntMap.empty

(* load all the external library files *)
let load_ext_libs st =
  let load st f = eval st (file_in f) in
  let st' = List.fold_left load st [ "lib/ext.ferret"
                                   ; "lib/io.ferret"
                                   ; "lib/math.ferret"
                                   ; "lib/pairs.ferret"
                                   ; "lib/lists.ferret"
                                   ; "lib/assocs.ferret"
                                   ]
  in
  { st' with Cell.env=user::st'.Cell.env }

(* display the message of the day *)
let motd () =
  let copy = "copyright (c) 2012 by jeffrey massung" in
  let rights = "all rights reserved" in
  let help = "use ctrl-c to interrupt and ctrl-d to quit" in
  Printf.printf "%sferret 0.6, %s, %s%s\n%s\n" cyan copy rights clear help

(* main *)
let _ =
  let st = Cell.new_thread Prims.prims in
  setup_term ();
  install_kill_signal ();
  Random.self_init ();
  motd ();
  repl (load_ext_libs st)

