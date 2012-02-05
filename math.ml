(* ferret math prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * math.ml
 *)

open Cell
open Interp

(* binary operation on two values *)
let prim_op f st xs =
  let (lval,xs') = pop1 xs in
  let (rval,xs') = pop1 xs' in
  f lval rval,xs'

(* binary comparison on two cells *)
let prim_cmp_op f =
  prim_op (fun x y -> Bool (f x y))

(* binary math on two numbers *)
let prim_binary_num_op f =
  prim_op (fun x y -> f (num_of_cell x) (num_of_cell y))

(* unary math on one number *)
let prim_unary_num_op f st xs =
  let (x,xs') = coerce num_of_cell (reduce1 st xs) in
  f x,xs'

(* binary floating-point operation *)
let prim_binary_float_op f =
  prim_op (fun x y -> Num (Float (f (float_of_cell x) (float_of_cell y))))

(* binary integer operation *)
let prim_binary_int_op f =
  prim_op (fun x y -> Num (Int (f (int_of_cell x) (int_of_cell y))))

(* unary floating-point operation *)
let prim_unary_float_op f st xs =
  let (x,xs') = coerce float_of_cell (reduce1 st xs) in
  Num (Float (f x)),xs'

(* unary integer operation *)
let prim_unary_int_op f st xs =
  let (x,xs') = coerce int_of_cell (reduce1 st xs) in
  Num (Int (f x)),xs'

(* equality test *)
let prim_eq st xs =
  let eq x y = try (compare_cell x y) = 0 with _ -> false in
  (prim_cmp_op eq) st xs

(* non-equality test *)
let prim_ne st xs =
  let ne x y = try (compare_cell x y) <> 0 with _ -> false in
  (prim_cmp_op ne) st xs

(* less-than equality test *)
let prim_lt st xs =
  let lt x y = (compare_cell x y) < 0 in
  (prim_cmp_op lt) st xs

(* greater-than equality test *)
let prim_gt st xs =
  let gt x y = (compare_cell x y) > 0 in
  (prim_cmp_op gt) st xs

(* less-than or equal test *)
let prim_le st xs =
  let le x y = (compare_cell x y) <= 0 in
  (prim_cmp_op le) st xs

(* greater-than or equal test *)
let prim_ge st xs =
  let ge x y = (compare_cell x y) >= 0 in
  (prim_cmp_op ge) st xs

(* binary math operation, chooses int or float op *)
let prim_binary_op i f st xs =
  let op x y =
    match x,y with
        (Int a,Int b) -> Num (Int (i a b))
      | (Int a,Float b) -> Num (Float (f (float_of_int a) b))
      | (Float a,Int b) -> Num (Float (f a (float_of_int b)))
      | (Float a,Float b) -> Num (Float (f a b))
  in
  (prim_binary_num_op op) st xs

(* unary math operation, choose int or float op *)
let prim_unary_op i f st xs =
  let op = function
    | Int x -> Num (Int (i x))
    | Float x -> Num (Float (f x))
  in
  (prim_unary_num_op op) st xs

(* simple numeric operations *)
let prim_plus st xs = (prim_binary_op ( + ) ( +. )) st xs
let prim_minus st xs = (prim_binary_op ( - ) ( -. )) st xs
let prim_times st xs = (prim_binary_op ( * ) ( *. )) st xs
let prim_divide st xs = (prim_binary_op ( / ) ( /. )) st xs
let prim_mod st xs = (prim_binary_op ( mod ) mod_float) st xs
let prim_abs st xs = (prim_unary_op abs abs_float) st xs
let prim_neg st xs = (prim_unary_op ( ~- ) ( ~-. )) st xs

(* integer-only and float-only binary operations *)
let prim_div st xs = (prim_binary_int_op ( / )) st xs
let prim_pow st xs = (prim_binary_float_op ( ** )) st xs

(* floating-point operations *)
let prim_sqrt st xs = (prim_unary_float_op sqrt) st xs
let prim_exp st xs = (prim_unary_float_op exp) st xs
let prim_log st xs = (prim_unary_float_op log) st xs
let prim_log10 st xs = (prim_unary_float_op log10) st xs
let prim_sin st xs = (prim_unary_float_op sin) st xs
let prim_cos st xs = (prim_unary_float_op cos) st xs
let prim_tan st xs = (prim_unary_float_op tan) st xs
let prim_asin st xs = (prim_unary_float_op asin) st xs
let prim_acos st xs = (prim_unary_float_op acos) st xs
let prim_atan st xs = (prim_unary_float_op atan) st xs
let prim_sinh st xs = (prim_unary_float_op sinh) st xs
let prim_cosh st xs = (prim_unary_float_op cosh) st xs
let prim_tanh st xs = (prim_unary_float_op tanh) st xs
let prim_ceil st xs = (prim_unary_float_op ceil) st xs
let prim_floor st xs = (prim_unary_float_op floor) st xs

(* bitwise operations *)
let prim_lnot st xs = (prim_unary_int_op ( lnot )) st xs
let prim_land st xs = (prim_binary_int_op ( land )) st xs
let prim_lor st xs = (prim_binary_int_op ( lor )) st xs
let prim_lxor st xs = (prim_binary_int_op ( lxor )) st xs
let prim_lsl st xs = (prim_binary_int_op ( lsl )) st xs
let prim_lsr st xs = (prim_binary_int_op ( lsr )) st xs
let prim_asr st xs = (prim_binary_int_op ( asr )) st xs

(* truncate a float *)
let prim_truncate st xs =
  let (x,xs') = coerce int_of_cell (reduce1 st xs) in
  Num (Int x),xs'

(* coerce an integer to a float *)
let prim_float st xs =
  let (x,xs') = coerce float_of_cell (reduce1 st xs) in
  Num (Float x),xs'

(* test for zero, compares very close as well *)
let prim_zero st xs =
  let zerop = function
    | Int n -> n = 0
    | Float n ->
      match classify_float n with
          FP_zero -> true
        | FP_subnormal -> true
        | _ -> false
  in
  let (n,xs') = coerce num_of_cell (reduce1 st xs) in
  Bool (zerop n),xs'

(* the floating-point epsilon constant (int-size specific) *)
let prim_epsilon st xs = Num (Float epsilon_float),xs

(* min/max integers and floats *)
let prim_min_int st xs = Num (Int min_int),xs
let prim_max_int st xs = Num (Int max_int),xs
let prim_min_float st xs = Num (Float min_float),xs
let prim_max_float st xs = Num (Float max_float),xs

(* random number generation *)
let prim_uniform st xs = Num (Float (Random.float 1.0)),xs
let prim_random st xs = (prim_unary_op Random.int Random.float) st xs
let prim_choice st xs = Bool (Random.bool ()),xs

