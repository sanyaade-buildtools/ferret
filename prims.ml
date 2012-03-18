(* ferret prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * prims.ml
 *)

open Cell

(* leave ferret *)
let prim_bye st = exit 0

(* primitives *)
let prims =
  [ ("bye",prim_bye)

    (* kernel prims *)
  ; ("modules",Core.prim_modules)
  ; ("words",Core.prim_words)
  ; ("abort",Core.prim_abort)
  ; ("clear",Core.prim_clear)
  ; ("i",Core.prim_i)
  ; ("type",Core.prim_type)
  ; ("clock",Core.prim_clock)

    (* stack shuffling prims *)
  ; ("dup",Core.prim_dup)
  ; ("drop",Core.prim_drop)
  ; ("swap",Core.prim_swap)
  ; ("over",Core.prim_over)
  ; ("nip",Core.prim_nip)
  ; ("rot",Core.prim_rot)
  ; ("-rot",Core.prim_rrot)
  ; ("2dup",Core.prim_2dup)
  ; ("2drop",Core.prim_2drop)
  ; ("2swap",Core.prim_2swap)
  ; ("2over",Core.prim_2over)
  ; ("2nip",Core.prim_2nip)
  ; ("push",Core.prim_push)
  ; ("pop",Core.prim_pop)

    (* block closure prims *)
  ; ("apply",Core.prim_apply)
  ; ("try",Core.prim_try)

    (* record functions *)
  ; ("make-tuple",Core.prim_make_tuple)
  ; ("element@",Core.prim_at)
  ; ("element!",Core.prim_set)

    (* list prims *)
  ; (",",Core.prim_cons)
  ; ("uncons",Core.prim_uncons)
  ; ("null",Core.prim_null)
  ; ("len",Core.prim_len)
  ; ("rev",Core.prim_rev)
  ; ("hd",Core.prim_hd)
  ; ("tl",Core.prim_tl)
  ; ("foldl",Core.prim_foldl)
  ; ("foldr",Core.prim_foldr)
  ; ("merge",Core.prim_merge)
  ; ("sort",Core.prim_sort)
  ; ("explode",Core.prim_explode)
  ; ("implode",Core.prim_implode)

    (* process prims *)
  ; ("spawn",Core.prim_spawn)
  ; ("self",Core.prim_self)
  ; ("kill",Core.prim_kill)
  ; ("status",Core.prim_status)
  ; ("join",Core.prim_join)
  ; ("sleep",Core.prim_sleep)
  ; ("yield",Core.prim_yield)
  ; ("!",Core.prim_send)
  ; ("@",Core.prim_receive)
    
    (* math prims *)
  ; ("=",Core.prim_eq)
  ; ("<>",Core.prim_ne)
  ; ("<",Core.prim_lt)
  ; (">",Core.prim_gt)
  ; ("<=",Core.prim_le)
  ; (">=",Core.prim_ge)
  ; ("+",Core.prim_plus)
  ; ("-",Core.prim_minus)
  ; ("*",Core.prim_times)
  ; ("/",Core.prim_divide)
  ; ("//",Core.prim_div)
  ; ("**",Core.prim_pow)
  ; ("mod",Core.prim_mod)
  ; ("abs",Core.prim_abs)
  ; ("neg",Core.prim_neg)
  ; ("1+",Core.prim_inc)
  ; ("1-",Core.prim_dec)
  ; ("2*",Core.prim_lshift)
  ; ("2/",Core.prim_rshift)
  ; ("~not",Core.prim_lnot)
  ; ("~and",Core.prim_land)
  ; ("~or",Core.prim_lor)
  ; ("~xor",Core.prim_lxor)
  ; ("lsl",Core.prim_lsl)
  ; ("lsr",Core.prim_lsr)
  ; ("asr",Core.prim_asr)
  ; ("sqrt",Core.prim_sqrt)
  ; ("exp",Core.prim_exp)
  ; ("log",Core.prim_log)
  ; ("log10",Core.prim_log10)
  ; ("sin",Core.prim_sin)
  ; ("cos",Core.prim_cos)
  ; ("tan",Core.prim_tan)
  ; ("asin",Core.prim_asin)
  ; ("acos",Core.prim_acos)
  ; ("atan",Core.prim_atan)
  ; ("sinh",Core.prim_sinh)
  ; ("cosh",Core.prim_cosh)
  ; ("tanh",Core.prim_tanh)
  ; ("ceil",Core.prim_ceil)
  ; ("floor",Core.prim_floor)
  ; ("truncate",Core.prim_truncate)
  ; ("float",Core.prim_float)
  ; ("0=",Core.prim_zero true)
  ; ("0<>",Core.prim_zero false)
  ; ("0<",Core.prim_negative)
  ; ("0>",Core.prim_positive)
  ; ("epsilon",Core.prim_epsilon)
  ; ("max-int",Core.prim_max_int)
  ; ("min-int",Core.prim_min_int)
  ; ("max-float",Core.prim_max_float)
  ; ("min-float",Core.prim_min_float)
  ; ("random",Core.prim_random)

    (* boolean operator prims *)
  ; ("not",Core.prim_not)
  ; ("and",Core.prim_and)
  ; ("or",Core.prim_or)

    (* string prims *)
  ; ("form",Core.prim_form)
  ; ("&",Core.prim_strcat)
  ; ("&&",Core.prim_strcat_space)
  ; ("upcase",Core.prim_uppercase)
  ; ("downcase",Core.prim_lowercase)
  ; ("/string",Core.prim_strip)

    (* output prims *)
  ; (".s",Core.prim_stack)
  ; (".cs",Core.prim_control_stack)
  ; ("putc",Core.prim_princ)
  ; ("puts",Core.prim_print print_endline)
  ; ("print",Core.prim_print print_string)
  ; ("prerr",Core.prim_print prerr_endline)

    (* io prims *) 
  ; ("file",Core.prim_file) 
  ; ("url",Core.prim_url)
  ; ("input",Core.prim_input)
  ; ("read",Core.prim_read)
  ; ("load",Core.prim_load)
  ]

