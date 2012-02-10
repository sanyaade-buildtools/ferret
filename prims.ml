(* ferret prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * prims.ml
 *)

open Cell

(* leave ferret *)
let prim_bye st xs = exit 0

(* primitives *)
let prims =
  [ ("bye",prim_bye)

    (* core prims *)
  ; ("reductions",Core.prim_reducs)
  ; ("fn",Core.prim_fn)
  ; ("let",Core.prim_let)
  ; ("return",Core.prim_return)
  ; ("make",Core.prim_make)
  ; ("!",Core.prim_get)
  ; ("->",Core.prim_call)
  ; ("fail",Core.prim_fail)
  ; ("if",Core.prim_if)
  ; ("while",Core.prim_while true)
  ; ("until",Core.prim_while false)
  ; ("not",Core.prim_not)
  ; ("and",Core.prim_and)
  ; ("or",Core.prim_or)
  ; ("apply",Core.prim_apply)
  ; ("do",Core.prim_do)
  ; ("reduce",Core.prim_reduce)
  ; ("load",Core.prim_load)
  ; ("forever",Core.prim_forever)
  ; ("for",Core.prim_for)
  ; ("foreach",Core.prim_foreach)

    (* series prims *)
  ; (",",Series.prim_cons)
  ; ("null?",Series.prim_null)
  ; ("head",Series.prim_head)
  ; ("tail",Series.prim_tail)
  ; ("length",Series.prim_len)
  ; ("reverse",Series.prim_rev)
  ; ("@",Series.prim_pair)
  ; ("fst",Series.prim_fst)
  ; ("snd",Series.prim_snd)

    (* io prims *)
  ; ("stdout",Io.prim_stdout)
  ; ("stdin",Io.prim_stdin)
  ; ("stderr",Io.prim_stderr)
  ; ("open",Io.prim_open)
  ; ("close",Io.prim_close)
  ; ("flush",Io.prim_flush)
  ; ("write",Io.prim_write)
  ; ("write-line",Io.prim_write_line)
  ; ("read",Io.prim_read)
  ; ("read-line",Io.prim_read_line)

    (* math prims *)
  ; ("=",Math.prim_eq)
  ; ("<>",Math.prim_ne)
  ; ("<",Math.prim_lt)
  ; (">",Math.prim_gt)
  ; ("<=",Math.prim_le)
  ; (">=",Math.prim_ge)
  ; ("+",Math.prim_plus)
  ; ("-",Math.prim_minus)
  ; ("*",Math.prim_times)
  ; ("/",Math.prim_divide)
  ; ("//",Math.prim_div)
  ; ("**",Math.prim_pow)
  ; ("mod",Math.prim_mod)
  ; ("abs",Math.prim_abs)
  ; ("neg",Math.prim_neg)
  ; ("lnot",Math.prim_lnot)
  ; ("land",Math.prim_land)
  ; ("lor",Math.prim_lor)
  ; ("lxor",Math.prim_lxor)
  ; ("lsl",Math.prim_lsl)
  ; ("lsr",Math.prim_lsr)
  ; ("asr",Math.prim_asr)
  ; ("sqrt",Math.prim_sqrt)
  ; ("exp",Math.prim_exp)
  ; ("log",Math.prim_log)
  ; ("log10",Math.prim_log10)
  ; ("sin",Math.prim_sin)
  ; ("cos",Math.prim_cos)
  ; ("tan",Math.prim_tan)
  ; ("asin",Math.prim_asin)
  ; ("acos",Math.prim_acos)
  ; ("atan",Math.prim_atan)
  ; ("sinh",Math.prim_sinh)
  ; ("cosh",Math.prim_cosh)
  ; ("tanh",Math.prim_tanh)
  ; ("ceil",Math.prim_ceil)
  ; ("floor",Math.prim_floor)
  ; ("truncate",Math.prim_truncate)
  ; ("float",Math.prim_float)
  ; ("zero?",Math.prim_zero)
  ; ("epsilon",Math.prim_epsilon)
  ; ("max-int",Math.prim_max_int)
  ; ("min-int",Math.prim_min_int)
  ; ("max-float",Math.prim_max_float)
  ; ("min-float",Math.prim_min_float)
  ; ("uniform",Math.prim_uniform)
  ; ("random",Math.prim_random)
  ; ("choice",Math.prim_choice)

    (* date/time prims *)
  ; ("clock",Time.prim_clock)

    (* string prims *)
  ; ("mold",Strings.prim_mold)
  ; ("form",Strings.prim_form)
  ; ("&",Strings.prim_strcat)
  ; ("&&",Strings.prim_strcat_space)
  ; ("toupper",Strings.prim_uppercase)
  ; ("tolower",Strings.prim_lowercase)
  ; ("sub",Strings.prim_sub)
  ]

(* create a dynamic environment from the list of prims *)
let prim_env =
  let env = Hashtbl.create 100 in
  let bind (s,p) =
    let f = Atom.intern s in
    Hashtbl.replace env f.Atom.i (Cell.Proc (Cell.Prim (f,p)))
  in
  List.iter bind prims;
  env

