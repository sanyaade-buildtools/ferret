(* ferret entry point
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * main.ml
 *)

open Term
open Interp

(* display the read prompt and read a line *)
let prompt i =
  Printf.printf "%s%d-%s " yellow i clear;
  read_line ()

(* read-eval-print loop *)
let rec repl st =
  try
    let i = ref 0 in
    while true do
      try
        let s = prompt !i in
        if String.length s > 0 then
        begin
          let x = eval st s in
          if x = Cell.Undef
          then Printf.printf "  %sok%s\n" cyan clear
          else Printf.printf "%s==%s %s\n" green clear (Cell.mold x);
          incr i
        end
      with 
          Sys.Break -> Printf.printf "\n%sInterrupt!%s\n" red clear
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
  load_ext_libs st;
  repl st 

