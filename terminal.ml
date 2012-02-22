(* ferret terminal
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * terminal.ml
 *)

open Cell

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

(* display an entire stack *)
let show_stack = function
  | [] -> Printf.printf "%sEmpty stack%s\n" blue clear
  | xs -> let show i x =
            let color = if i = 0 then cyan else blue in
            Printf.printf "%s[ +%d ]%s %s\n" color i clear (mold x);
            i+1
          in
          ignore (List.fold_left show 0 xs)

(* display a stack in reverse contents *)
let show_rev_stack xs =
  let xs' = List.rev xs in
  let len = List.length xs in
  let show i x =
    Printf.printf "%s[ -%d ]%s %s\n" blue i clear (mold x);
    i-1
  in
  ignore (List.fold_left show len xs')

