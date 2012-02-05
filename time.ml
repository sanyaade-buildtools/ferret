(* ferret date/time prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * io.ml
 *)

open Cell
open Interp

(* clock timer *)
let prim_clock st xs =
  Num (Float (Sys.time ())),xs

