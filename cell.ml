(* ferret types
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * types.ml
 *)

type t =
    Undef
  | Quote of t
  | Word of Word.t
  | Proc of proc
  | Bool of bool
  | Char of char
  | Str of string
  | Num of num
  | Block of t list
  | Expr of t list
  | Port_in of in_channel * string
  | Port_out of out_channel * string

(* numeric values *)
and num =
  | Int of int
  | Float of float

(* procedures *)
and proc = 
    Closure of local_env * Atom.t list * t list
  | Prim of Atom.t * native

(* dynamically scoped and lexically scoped environments *)
and dynamic_env = (int, t) Hashtbl.t
and local_env = (int * t ref) list

(* primitive function *)
and native = st -> t list -> t * t list

(* thread state *)
and st =
    { env    : dynamic_env
    ; locals : local_env
    ; pinfo  : process_info
    }

(* coroutine process info *)
and process_info =
    { mailbox : t Queue.t
    ; status  : process_result option ref
    }

(* corouting result *)
and process_result =
    Completed of t
  | Terminated of exn

(* type exceptions *)
exception Not_a_block of t
exception Not_a_boolean of t
exception Not_a_char of t
exception Not_a_float of t
exception Not_a_int of t
exception Not_a_number of t
exception Not_a_pair of t
exception Not_a_port of t
exception Not_a_proc of t
exception Not_a_string of t
exception Not_a_undef of t
exception Not_a_var of t
exception Not_a_word of t
exception Uncomparable_type

(* create a new coroutine process *)
let new_process () =
  { mailbox=Queue.create ()
  ; status=ref None
  }

(* create a new thread state *)
let new_thread env =
  { env=env
  ; locals=[]
  ; pinfo=new_process ()
  }

(* spawn a new coroutine off a process *)
let spawn_thread st = 
  { st with locals=[]; pinfo=new_process () }

(* convert a cell to a readable string *)
let rec mold = function
  | Undef -> "none"
  | Quote x -> "'" ^ mold x
  | Word (Word.Sym s) -> s.Atom.name
  | Word (Word.Binary (Word.Op,s)) -> s.Atom.name
  | Word (Word.Binary (Word.Infix,s)) -> "`" ^ s.Atom.name ^ "`"
  | Word (Word.Getter s) -> ":" ^ s.Atom.name
  | Word (Word.Var s) -> s.Atom.name ^ ":"
  | Proc (Closure (_,ps,xs)) -> mold_fn ps xs
  | Proc (Prim (p,_)) -> p.Atom.name
  | Bool false -> "false"
  | Bool true -> "true"
  | Char c -> "#\"" ^ (Char.escaped c) ^ "\""
  | Str s -> "\"" ^ (String.escaped s) ^ "\""
  | Num (Int i) -> string_of_int i
  | Num (Float f) -> string_of_float f
  | Block xs -> "[" ^ (mold_list xs) ^ "]"
  | Expr xs -> "(" ^ (mold_list xs) ^ ")"
  | Port_in (_,s) -> mold_unreadable_obj "port in" s
  | Port_out (_,s) -> mold_unreadable_obj "port out" s

(* convert a list of cells to a string *)
and mold_list xs = String.concat " " (List.map mold xs)

(* convert a closure to a string *)
and mold_fn ps xs = 
  Printf.sprintf "fn [%s] [%s]" (mold_atoms ps) (mold_list xs)

(* create a string from a list of atoms *)
and mold_atoms ps =
  String.concat " " (List.map (fun a -> a.Atom.name) ps)

(* create a string to print for an unreadable object *)
and mold_unreadable_obj =
  Printf.sprintf "#<%s %s>"

(* coerce function *)
let coerce f (x,xs) = (f x,xs)

(* atom coercion *)
let atom_of_cell = function
  | Word (Word.Sym x) -> x
  | Word (Word.Binary (_,x)) -> x
  | x -> raise (Not_a_word x)

(* block coercion *)
let list_of_cell = function
  | Block xs -> xs
  | Expr xs -> xs
  | x -> raise (Not_a_block x)

(* boolean coercion *)
let bool_of_cell = function
  | Undef -> false
  | Bool x -> x
  | x -> raise (Not_a_boolean x)

(* character coercion *)
let char_of_cell = function
  | Char x -> x
  | x -> raise (Not_a_char x)

(* float coercion *)
let float_of_cell = function
  | Num (Float x) -> x
  | Num (Int x) -> float_of_int x
  | x -> raise (Not_a_float x)

(* input channel coercion *)
let in_chan_of_cell = function
  | Port_in (h,_) -> h
  | x -> raise (Not_a_port x)

(* integer coercion *)
let int_of_cell = function
  | Num (Int x) -> x
  | Num (Float x) -> int_of_float x
  | x -> raise (Not_a_int x)

(* number coercion *)
let num_of_cell = function
  | Num x -> x
  | x -> raise (Not_a_number x)

(* option coercion *)
let option_of_cell = function
  | Undef -> None
  | x -> Some x

(* output channel coercion *)
let out_chan_of_cell = function
  | Port_out (h,_) -> h
  | x -> raise (Not_a_port x)

(* pair coercion *)
let pair_of_cell = function
  | Block [a;b] -> a,b
  | x -> raise (Not_a_pair x)

(* procedure coercion *)
let proc_of_cell = function
  | Proc p -> p
  | x -> raise (Not_a_proc x)

(* string coercion *)
let string_of_cell = function
  | Str x -> x
  | x -> raise (Not_a_string x)

(* var coercion *)
let var_of_cell = function
  | Word (Word.Var x) -> x
  | x -> raise (Not_a_var x)

(* symbol coercion *)
let sym_of_cell = function
  | Word (Word.Sym x) -> x
  | x -> raise (Not_a_word x)

(* compare function *)
let rec compare_cell = function
  | Block a -> fun b -> compare_list a (list_of_cell b)
  | Bool a -> fun b -> compare a (bool_of_cell b)
  | Char a -> fun b -> compare a (char_of_cell b)
  | Expr a -> fun b -> compare_list a (list_of_cell b)
  | Num a -> fun b -> compare_num a (num_of_cell b)
  | Port_in (_,_) -> raise Uncomparable_type
  | Port_out (_,_) -> raise Uncomparable_type
  | Proc a -> raise Uncomparable_type
  | Quote a -> fun b -> compare_cell a b
  | Str a -> fun b -> compare a (string_of_cell b)
  | Word a -> fun b -> Atom.compare (Word.atom a) (atom_of_cell b)
  | Undef -> function
      | Undef -> 0
      | x -> raise (Not_a_undef x)

(* compare a list of cells *)
and compare_list a b =
  match a,b with
      ([],[]) -> 0
    | ([],_) -> -1
    | (_,[]) -> 1
    | (x::xs,y::ys) ->
      match compare_cell x y with
          0 -> compare_list xs ys
        | x -> x

(* compare numerics *)
and compare_num a b =
  match a,b with
      (Int x,Int y) -> compare x y
    | (Int x,Float y) -> compare (float_of_int x) y
    | (Float x,Int y) -> compare x (float_of_int y)
    | (Float x,Float y) -> compare x y

(* override compare *)
let compare = compare_cell

