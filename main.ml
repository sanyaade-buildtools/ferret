(* ferret entry point
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * main.ml
 *)

open Term
open Interp

(* the last returned value *)
let it = Atom.intern "it"

(* print the output of a value *)
let show = function
  | Cell.Undef -> Printf.printf "  %sok%s\n" cyan clear
  | x -> Printf.printf "%s==%s %s\n" green clear (Cell.mold x)

(* read-eval-print loop *)
let rec repl st =
  try
    while true do
      try
        let x = eval st (read_line ()) in
        bind st it x;
        show x
      with 
          Sys.Break -> Printf.printf "\n** %sInterrupt!%s\n" red clear
        | End_of_file -> raise End_of_file
        | e -> Printf.printf "%s** %s%s\n" red (Printexc.to_string e) clear;
    done
  with End_of_file -> ()

(* load all the external library files *)
let load_ext_libs st =
  List.iter (import st) [ "lib/core.ferret"
                        ; "lib/lists.ferret"
                        ; "lib/math.ferret"
                        ; "lib/io.ferret"
                        ]

(* display the message of the day *)
let motd () =
  let copy = "copyright (c) 2012 by jeffrey massung" in
  let rights = "all rights reserved" in
  let help = "use ctrl-c to interrupt and ctrl-d to quit" in
  Printf.printf "%sferret 0.4, %s, %s%s\n%s\n" cyan copy rights clear help

(* main *)
let _ =
  let st = Cell.new_thread Prims.prim_env in
  setup_term ();
  Process.install_kill_signal ();
  Random.self_init ();
  motd ();
  bind st it Cell.Undef;
  load_ext_libs st;
  show Cell.Undef;
  repl st 

