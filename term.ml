(* ferret terminal
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * term.ml
 *)

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

