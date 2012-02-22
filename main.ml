(* ferret entry point
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * main.ml
 *)

open Interp

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

(* terminal ansi colors *)
let red = "\x1b[31m"
let green = "\x1b[32m"
let yellow = "\x1b[33m"
let blue = "\x1b[34m"
let magenta = "\x1b[35m"
let cyan = "\x1b[36m"
let gray = "\x1b[37m"
let clear = "\x1b[0m"

(* ready the terminal to handle interrupts *)
let setup_term () =
  Sys.interactive := true;
  Sys.catch_break true

(* display the ok prompt *)
let show_ok () = Printf.printf "  %sok%s\n" cyan clear

(* print the output of a value *)
let show_top stack = 
  let show x = Printf.sprintf "%s==%s %s" green clear (Cell.mold x) in 
  let rest xs = Printf.sprintf "%s (+ %d)%s" yellow (List.length xs) clear in
  match stack with
      [] -> show_ok ()
    | [x] -> Printf.printf "%s\n" (show x)
    | x::xs -> Printf.printf "%s%s\n" (show x) (rest xs)

(* print an exception to stderr *)
let show_err = function
  | Sys.Break -> Printf.printf "\n%s** Interrupt!%s\n" red clear
  | e -> Printf.printf "%s** %s%s\n" red (Printexc.to_string e) clear

(* read-eval-print loop *)
let rec repl st =
  try
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
(*
(* load all the external library files *)
let load_ext_libs st =
  List.iter (import st) [ "lib/core.ferret"
                        ; "lib/lists.ferret"
                        ; "lib/math.ferret"
                        ; "lib/io.ferret"
                        ]
*)
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
  show_ok ();
  repl st 

