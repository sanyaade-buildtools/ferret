(* ferret core types
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * cell.ml
 *)

type t =
  | Atom of Atom.t
  | Block of local_env * xt list
  | Bool of bool
  | Char of char
  | Filespec of filespec
  | List of t list
  | Num of num
  | Pid of Thread.t * process_info
  | Str of string

(* location spec *)
and filespec =
  | File of string
  | Url of Neturl.url

(* channel ports *)
and port =
  | Port_in of in_channel * string
  | Port_out of out_channel * string

(* numeric values *)
and num =
  | Float of float
  | Int of int

(* executable tokens *)
and xt = 
  | Word of Atom.t * word
  | Local of Atom.t
  | With of Atom.t list * xt list
  | If of xt list * xt list
  | While of xt list * xt list
  | Until of xt list
  | Loop of xt list
  | For of xt list
  | Each of xt list
  | Lit of t
  | Exit
  | Recurse

(* dictionary entry *)
and word =
    { def : def
    }

(* user-defined and native procedures *)
and def =
  | Colon of xt list
  | Prim of native

(* primitive function *)
and native = st -> st

(* dynamically scoped and lexically scoped environments *)
and dynamic_env = word Atom.IntMap.t
and local_env = (int * t) list

(* a dictionary is a list of named environments *)
and dict = (Atom.t * dynamic_env) list

(* thread state *)
and st =
    { env    : dict
    ; locals : local_env
    ; stack  : t list
    ; cs     : t list
    ; pinfo  : process_info
    ; reducs : int ref
    ; i      : t option
    }

(* coroutine process info *)
and process_info =
    { mb     : t Queue.t
    ; status : process_result Mvar.t
    ; lock   : Mutex.t
    }

(* corouting result *)
and process_result =
    Completed of st
  | Terminated of exn

(* type exceptions *)
exception Not_a_atom of t
exception Not_a_block of t
exception Not_a_boolean of t
exception Not_a_char of t
exception Not_a_file of t
exception Not_a_float of t
exception Not_a_int of t
exception Not_a_list of t
exception Not_a_number of t
exception Not_a_pair of t
exception Not_a_port of t
exception Not_a_port_in of t
exception Not_a_port_out of t
exception Not_a_process of t
exception Not_a_spec of t
exception Not_a_string of t
exception Not_a_url of t

(* compare exceptions *)
exception Uncomparable_type

(* create a new coroutine process *)
let new_process () =
  { mb=Queue.create ()
  ; status=Mvar.empty ()
  ; lock=Mutex.create ()
  }

(* create a new thread state *)
let new_thread env =
  let bind m (s,p) = Atom.IntMap.add (Atom.intern s).Atom.i { def=Prim p } m in
  let core = List.fold_left bind Atom.IntMap.empty env in
  { env=[Atom.intern "Core",core]
  ; locals=[]
  ; stack=[]
  ; cs=[]
  ; pinfo=new_process ()
  ; reducs=ref 0
  ; i=None
  }

(* spawn a new coroutine off a process *)
let spawn_thread st = 
  { env=st.env
  ; locals=st.locals
  ; stack=[]
  ; cs=[]
  ; pinfo=new_process ()
  ; reducs=ref 0 
  ; i=None
  }

(* convert a cell to a readable string *)
let rec mold = function
  | Atom atom -> atom.Atom.name
  | Block (env,xs) -> Printf.sprintf "{%s}" (mold_block env xs)
  | Bool false -> "F"
  | Bool true -> "T"
  | Char c -> Printf.sprintf "'%s'" (Char.escaped c)
  | Filespec (File f) -> mold_unreadable_obj "file" f
  | Filespec (Url url) -> mold_unreadable_obj "url" (Neturl.string_of_url url)
  | List xs -> Printf.sprintf "[%s]" (mold_list xs)
  | Num (Float f) -> string_of_float f
  | Num (Int i) -> string_of_int i
  | Pid (pid,_) -> mold_unreadable_obj "pid" (string_of_int (Thread.id pid))
  | Str s -> Printf.sprintf "\"%s\"" (String.escaped s)

(* convert a block to a string *)
and mold_block env xs =
  let mold_xt = function
    | Word (atom,_) -> mold_atom env atom
    | Local atom -> mold_atom env atom
    | With (ps,xs) -> mold_env env ps xs
    | If (ts,[]) -> mold_flow1 env "if" ts "then"
    | If (ts,es) -> mold_flow2 env "if" ts "else" es "then"
    | While (ts,es) -> mold_flow2 env "begin" ts "while" es "repeat"
    | Until xs -> mold_flow1 env "begin" xs "until"
    | Loop xs -> mold_flow1 env "begin" xs "again"
    | For xs -> mold_flow1 env "for" xs "next"
    | Each xs -> mold_flow1 env "each" xs "next"
    | Lit x -> mold x
    | Exit -> "exit"
    | Recurse -> "recurse"
  in
  String.concat " " (List.map mold_xt xs)  

(* convert an atom to its lexical value or print it *)
and mold_atom env p =
  try mold (List.assq p.Atom.i env) with Not_found -> p.Atom.name

(* convert a lexical scope to a string *)
and mold_env env ps xs =
  let args = String.concat " " (List.map (fun p -> p.Atom.name) ps) in
  String.concat " " ["with";args;"->";mold_block env xs]

(* e.g. if .. then *)
and mold_flow1 env start xs close =
  String.concat " " [start;mold_block env xs;close]

(* e.g. if .. else .. then *)
and mold_flow2 env start ts mid es close =
  String.concat " " [start;mold_block env ts;mid;mold_block env es;close]

(* convert a list of cells to a string *)
and mold_list xs = String.concat " " (List.map mold xs)

(* create a string to print for an unreadable object *)
and mold_unreadable_obj =
  Printf.sprintf "<%s:%s>"

(* coerce function *)
let coerce f (x,st) = (f x,st)

(* atom coercion *)
let atom_of_cell = function
  | Atom atom -> atom
  | x -> raise (Not_a_atom x)

(* block coercion *)
let block_of_cell = function
  | Block (env,xts) -> env,xts
  | x -> raise (Not_a_block x)

(* boolean coercion *)
let bool_of_cell = function
  | Bool x -> x
  | x -> raise (Not_a_boolean x)

(* character coercion *)
let char_of_cell = function
  | Char x -> x
  | x -> raise (Not_a_char x)

(* file coercion *)
let file_of_cell = function
  | Filespec (File f) -> f
  | x -> raise (Not_a_file x)

(* float coercion *)
let float_of_cell = function
  | Num (Float x) -> x
  | Num (Int x) -> float_of_int x
  | x -> raise (Not_a_float x)
(*
(* input channel coercion *)
let in_chan_of_cell = function
  | Port_in (h,_) -> h
  | x -> raise (Not_a_port x)
*)
(* integer coercion *)
let int_of_cell = function
  | Num (Int x) -> x
  | x -> raise (Not_a_int x)

(* list coercion *)
let list_of_cell = function
  | List xs -> xs
  | x -> raise (Not_a_list x)

(* number coercion *)
let num_of_cell = function
  | Num x -> x
  | x -> raise (Not_a_number x)
(*
(* output channel coercion *)
let out_chan_of_cell = function
  | Port_out (h,_) -> h
  | x -> raise (Not_a_port x)
*)
(*
(* pair coercion *)
let pair_of_cell = function
  | Pair (a,b) -> a,b
  | x -> raise (Not_a_pair x)
*)

(* spec coercion *)
let spec_of_cell = function
  | Filespec x -> x
  | x -> raise (Not_a_spec x)

(* string coercion *)
let string_of_cell = function
  | Str x -> x
  | x -> raise (Not_a_string x)

(* thread coercion *)
let thread_of_cell = function
  | Pid (thread,pinfo) -> thread,pinfo
  | x -> raise (Not_a_process x)

(* url coercion *)
let url_of_cell = function
  | Filespec (Url url) -> url
  | x -> raise (Not_a_url x)

(* compare function *)
let rec compare_cell = function
  | Atom a -> fun b -> Atom.compare a (atom_of_cell b)
  | Bool a -> fun b -> compare a (bool_of_cell b)
  | Char a -> fun b -> compare a (char_of_cell b)
  | Filespec a -> fun b -> compare_spec a (spec_of_cell b)
  | List a -> fun b -> compare_list a (list_of_cell b)
  | Num a -> fun b -> compare_num a (num_of_cell b)
  | Str a -> fun b -> compare a (string_of_cell b)
  | _ -> raise Uncomparable_type

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

(* compare two pairs *)
and compare_pair (a1,a2) (b1,b2) =
  match compare_cell a1 b1 with
      0 -> compare_cell a2 b2
    | x -> x

(* compare numerics *)
and compare_num a b =
  match a,b with
      (Int x,Int y) -> compare x y
    | (Int x,Float y) -> compare (float_of_int x) y
    | (Float x,Int y) -> compare x (float_of_int y)
    | (Float x,Float y) -> compare x y

(* compare two location specs *)
and compare_spec a b =
  match a,b with
      (File x,File y) -> compare x y
    | (Url x,Url y) -> compare x y
    | (_,_) -> raise Uncomparable_type

(* override compare *)
let compare = compare_cell

