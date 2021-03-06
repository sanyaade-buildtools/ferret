(* ferret core prims
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * core.ml
 *)

open Cell
open Http_client.Convenience
open Interp

exception Abort of string
exception Control_stack_underflow
exception Empty_list
exception No_iterator

(* kill signal *)
let kill_lock = Mutex.create ()
let kill_sig = ref None

(* list the current modules *)
let prim_modules st =
  let xs = List.map (fun (m,_) -> Atom m) st.env in
  { st with stack=List xs::st.stack }

(* list all the words in the dictionary, top modules shadow *)
let prim_words st =
  let accum k v map = Atom.IntMap.add k v map in
  let fold xs (_,map) = Atom.IntMap.fold accum map xs in
  let dict = List.fold_left fold Atom.IntMap.empty st.env in
  let words = Atom.IntMap.bindings dict in
  let names = List.map (fun (_,w) -> Str w.word.Atom.name) words in
  { st with stack=List (List.sort compare names)::st.stack }

(* raise a failure assertion *)
let prim_abort st =
  let (s,_) = coerce string_of_cell (pop st) in raise (Abort s)

(* clear the stacks *)
let prim_clear st =
  { st with stack=[]; cs=[] }

(* remove the top stack item *)
let prim_drop st = 
  match st.stack with
      _::xs -> { st with stack=xs }
    | _ -> raise Stack_underflow

(* duplicate the top stack item *)
let prim_dup st =
  match st.stack with
      x::xs -> { st with stack=x::x::xs }
    | _ -> raise Stack_underflow

(* swap the top two stack items *)
let prim_swap st =
  match st.stack with
      a::b::xs -> { st with stack=b::a::xs }
    | _ -> raise Stack_underflow

(* duplicate the second stack item *)
let prim_over st =
  match st.stack with
      a::b::xs -> { st with stack=b::a::b::xs }
    | _ -> raise Stack_underflow

(* drop the second stack item *)
let prim_nip st =
  match st.stack with
      a::_::xs -> { st with stack=a::xs }
    | _ -> raise Stack_underflow

(* rotate the top 3 stack items *)
let prim_rot st =
  match st.stack with
      a::b::c::xs -> { st with stack=c::a::b::xs }
    | _ -> raise Stack_underflow

(* reverse rotate the top 3 stack items *)
let prim_rrot st =
  match st.stack with
      a::b::c::xs -> { st with stack=b::c::a::xs }
    | _ -> raise Stack_underflow

(* duplicate the top two stack items *)
let prim_2dup st =
  match st.stack with
      a::b::xs -> { st with stack=a::b::a::b::xs }
    | _ -> raise Stack_underflow

(* remove the top two stack items *)
let prim_2drop st =
  match st.stack with
      _::_::xs -> { st with stack=xs }
    | _ -> raise Stack_underflow

(* swap the top 4 stack items as pairs *)
let prim_2swap st =
  match st.stack with
      a::b::c::d::xs -> { st with stack=c::d::a::b::xs }
    | _ -> raise Stack_underflow

(* duplicate the 3rd and 4th stack items *)
let prim_2over st =
  match st.stack with
      a::b::c::d::xs -> { st with stack=c::d::a::b::c::d::xs }
    | _ -> raise Stack_underflow

(* drop the 3rd and 4th stack items *)
let prim_2nip st =
  match st.stack with
      a::b::_::_::xs -> { st with stack=a::b::xs }
    | _ -> raise Stack_underflow

(* push the top of the stack to the control stack *)
let prim_push st =
  match st.stack with
      x::xs -> { st with stack=xs; cs=x::st.cs }
    | _ -> raise Stack_underflow

(* pop the top of the control stack *)
let prim_pop st =
  match st.cs with
      c::cs -> { st with stack=c::st.stack; cs=cs }
    | _ -> raise Control_stack_underflow

(* apply a block *)
let prim_apply st =
  match st.stack with
      b::xs -> apply { st with stack=xs } b
    | _ -> raise Stack_underflow

(* attempt to apply a block, if it fails, catch the exception *)
let prim_try st =
  match st.stack with
      [] -> raise Stack_underflow
    | b::xs -> 
      try 
        let st' = apply { st with stack=xs } b in
        { st' with stack=Bool true::st'.stack }
      with _ -> { st with stack=Bool false::xs } 

(* create a new tuple instance *) 
let prim_make_tuple st =
  match st.stack with
      i::xs -> Compiler.make_tuple (int_of_cell i) { st with stack=xs }
    | _ -> raise Stack_underflow 

(* get an element in a tuple *)
let prim_at st =
  let index r i = (tuple_of_cell r).(int_of_cell i) in
  match st.stack with
      i::r::xs -> { st with stack=index r i::xs }
    | _ -> raise Stack_underflow

(* create a copy of a record, mutate an index, return copy *)
let prim_set st =
  let mut r i x = 
    let arr = tuple_of_cell r in
    let copy = Array.copy arr in
    copy.(int_of_cell i) <- x;
    Tuple copy
  in
  match st.stack with
      i::x::r::xs -> { st with stack=mut r i x::xs }
    | _ -> raise Stack_underflow

(* pair top two elements *)
let prim_pair st =
  match st.stack with
      s::f::xs -> { st with stack=Pair (f,s)::xs }
    | _ -> raise Stack_underflow

(* split pair elements *)
let prim_unpair st =
  match st.stack with
      p::xs -> let f,s = pair_of_cell p in { st with stack=s::f::xs }
    | _ -> raise Stack_underflow

(* get the fist element of a pair *)
let prim_fst st =
  match st.stack with
      p::xs -> let f,_ = pair_of_cell p in { st with stack=f::xs }
    | _ -> raise Stack_underflow

(* get the second element of a pair *)
let prim_snd st =
  match st.stack with
      p::xs -> let _,s = pair_of_cell p in { st with stack=s::xs }
    | _ -> raise Stack_underflow

(* create a new tuple with a new fst element *)
let prim_set_fst st =
  match st.stack with
      x::p::xs -> let _,s = pair_of_cell p in { st with stack=Pair (x,s)::xs }
    | _ -> raise Stack_underflow

(* create a new tuple with a new snd element *)
let prim_set_snd st =
  match st.stack with
      x::p::xs -> let f,_ = pair_of_cell p in { st with stack=Pair (f,x)::xs }
    | _ -> raise Stack_underflow

(* push the current iterator *)
let prim_i st =
  match st.i with
      None -> raise No_iterator
    | Some x -> { st with stack=x::st.stack }

(* return the type of a cell *)
let prim_type st =
  let typeof = function
    | Atom _ -> Atom.intern "Atom"
    | Block (_,_) -> Atom.intern "Block"
    | Bool _ -> Atom.intern "Bool"
    | Char _ -> Atom.intern "Char"
    | Filespec (File _) -> Atom.intern "File"
    | Filespec (Url _) -> Atom.intern "Url"
    | List _ -> Atom.intern "List"
    | Num (Float _) -> Atom.intern "Float"
    | Num (Int _) -> Atom.intern "Int"
    | Pair (_,_) -> Atom.intern "Pair"
    | Pid (_,_) -> Atom.intern "Pid"
    | Re (_,_) -> Atom.intern "Re"
    | Str _ -> Atom.intern "String"
    | Tuple _ -> Atom.intern "Tuple"
    | Unit -> Atom.intern "Unit"
  in
  match st.stack with
      x::xs -> { st with stack=Atom (typeof x)::xs }
    | _ -> raise Stack_underflow

(* return the time elapsed in this thread *)
let prim_clock st =
  { st with stack=Num (Float (Sys.time ()))::st.stack }

(* insert a new element at the head of a list *)
let prim_cons st =
  match st.stack with
      k::ks::xs -> { st with stack=List (k::list_of_cell ks)::xs }
    | _ -> raise Stack_underflow

(* split a list into the head and tail *)
let prim_uncons st =
  let (xs,st') = coerce list_of_cell (pop st) in
  match xs with
      [] -> raise Empty_list
    | (k::ks) -> { st' with stack=k::List ks::st'.stack }

(* true if a list is empty *)
let prim_null st =
  match st.stack with
      List []::xs -> { st with stack=Bool true::xs }
    | x::xs -> { st with stack=Bool false::List (list_of_cell x)::xs }
    | _ -> raise Stack_underflow

(* get the length of a list *)
let prim_len st =
  match st.stack with
      x::xs -> { st with stack=Num (Int (List.length (list_of_cell x)))::xs }
    | _ -> raise Stack_underflow

(* reverse a list *)
let prim_rev st =
  match st.stack with
      x::xs -> { st with stack=List (List.rev (list_of_cell x))::xs }
    | _ -> raise Stack_underflow

(* get the head of a list *)
let prim_hd st =
  let (xs,st') = coerce list_of_cell (pop st) in
  match xs with
      x::_ -> { st' with stack=x::st'.stack }
    | _ -> raise Empty_list

(* get the tail of a list *)
let prim_tl st =
  let (xs,st') = coerce list_of_cell (pop st) in
  match xs with
      _::x -> { st' with stack=List x::st'.stack }
    | _ -> raise Empty_list

(* reduce a list *)
let prim_foldl st =
  let fold st xt xs =
    List.fold_left (fun st x -> apply { st with stack=x::st.stack } xt) st xs
  in
  match st.stack with
      xt::a::l::xs -> fold { st with stack=a::xs } xt (list_of_cell l)
    | _ -> raise Stack_underflow

(* reduce a list in reverse order *)
let prim_foldr st =
  let fold st xt xs =
    List.fold_right (fun x st -> apply { st with stack=x::st.stack } xt) xs st
  in
  match st.stack with
      xt::a::l::xs -> fold { st with stack=a::xs } xt (list_of_cell l)
    | _ -> raise Stack_underflow

(* merge two lists in ascending order *)
let prim_merge st =
  match st.stack with
      a::b::xs -> let a' = list_of_cell a in
                  let b' = list_of_cell b in
                  { st with stack=List (List.merge compare_cell a' b')::xs }
    | _ -> raise Stack_underflow

(* sort a list in ascending order *)
let prim_sort st =
  match st.stack with
      x::xs -> let x' = list_of_cell x in
               { st with stack=List (List.sort compare_cell x')::xs }
    | _ -> raise Stack_underflow

(* explode a list onto the stack *)
let prim_explode st =
  match st.stack with
      x::xs -> { st with stack=(list_of_cell x) @ xs }
    | _ -> raise Stack_underflow

(* convert the stack into a list *)
let prim_implode st =
  { st with stack=[List st.stack] }

(* spawn a new process *)
let prim_spawn st =
  let (b,st') = coerce block_of_cell (pop st) in
  { st' with stack=(fork_thread (spawn_thread st') b)::st'.stack }

(* push the current process id *)
let prim_self st =
  { st with stack=Pid (Thread.self (),st.pinfo)::st.stack }

(* kill a process *)
let prim_kill st =
  let ((t,pinfo),st') = coerce thread_of_cell (pop st) in
  Mutex.lock kill_lock;
  kill_sig := Some (Thread.id t);
  ignore (Thread.join t);
  Mutex.unlock kill_lock;
  st'

(* return the current status of a thread *)
let prim_status st =
  let ((_,pinfo),st') = coerce thread_of_cell (pop st) in
  let running = Atom.intern "Running" in
  let completed = Atom.intern "Completed" in
  let terminated = Atom.intern "Terminated" in
  if Mvar.is_empty pinfo.status
  then { st' with stack=Atom running::st'.stack }
  else begin
    match Mvar.read pinfo.status with
        Completed _ -> { st' with stack=Atom completed::st'.stack }
      | Terminated _ -> { st' with stack=Atom terminated::st'.stack }
  end

(* wait for a process to complete *)
let prim_join st =
  let ((t,pinfo),st') = coerce thread_of_cell (pop st) in
  Thread.join t;
  match Mvar.read pinfo.status with
      Completed st -> { st' with stack=Bool true::List st.stack::st'.stack }
    | Terminated e -> { st' with stack=Bool false::st'.stack }

(* sleep this thread for a period of time *)
let prim_sleep st =
  let (sec,st') = coerce float_of_cell (pop st) in
  Thread.delay sec;
  st'

(* let other threads execute *)
let prim_yield st =
  Thread.yield ();
  st

(* send a message to another process *)
let prim_send st =
  let ((_,pinfo),st') = coerce thread_of_cell (pop st) in
  let (msg,st') = pop st' in
  Mutex.lock pinfo.lock;
  Queue.add msg pinfo.mb;
  Mutex.unlock pinfo.lock;
  st'

(* receive a message from another process *)
let rec prim_receive st =
  Mutex.lock st.pinfo.lock;
  while Queue.is_empty st.pinfo.mb do
    Mutex.unlock st.pinfo.lock;
    Thread.delay 0.1;
    Mutex.lock st.pinfo.lock
  done;
  let x = Queue.take st.pinfo.mb in
  Mutex.unlock st.pinfo.lock;
  { st with stack=x::st.stack }

(* binary operation on two values *)
let prim_op f st =
  match st.stack with
      rval::lval::xs -> { st with stack=f lval rval::xs }
    | _ -> raise Stack_underflow

(* binary comparison on two cells *)
let prim_cmp_op f =
  prim_op (fun x y -> Bool (f x y))

(* binary math on two numbers *)
let prim_binary_num_op f =
  prim_op (fun x y -> f (num_of_cell x) (num_of_cell y))

(* unary math on one number *)
let prim_unary_num_op f st =
  match st.stack with
      x::xs -> { st with stack=f (num_of_cell x)::xs }
    | _ -> raise Stack_underflow

(* binary floating-point operation *)
let prim_binary_float_op f =
  prim_op (fun x y -> Num (Float (f (float_of_cell x) (float_of_cell y))))

(* binary integer operation *)
let prim_binary_int_op f =
  prim_op (fun x y -> Num (Int (f (int_of_cell x) (int_of_cell y))))

(* unary floating-point operation *)
let prim_unary_float_op f st =
  match st.stack with
      x::xs -> { st with stack=Num (Float (f (float_of_cell x)))::xs }
    | _ -> raise Stack_underflow

(* unary integer operation *)
let prim_unary_int_op f st =
  match st.stack with
      x::xs -> { st with stack=Num (Int (f (int_of_cell x)))::xs }
    | _ -> raise Stack_underflow

(* equality test *)
let prim_eq st =
  let eq x y = try x == y || (compare_cell x y) = 0 with _ -> false in
  (prim_cmp_op eq) st

(* non-equality test *)
let prim_ne st =
  let ne x y = try x != y && (compare_cell x y) <> 0 with _ -> false in
  (prim_cmp_op ne) st

(* less-than equality test *)
let prim_lt st =
  let lt x y = (compare_cell x y) < 0 in
  (prim_cmp_op lt) st

(* greater-than equality test *)
let prim_gt st =
  let gt x y = (compare_cell x y) > 0 in
  (prim_cmp_op gt) st 

(* less-than or equal test *)
let prim_le st =
  let le x y = (compare_cell x y) <= 0 in
  (prim_cmp_op le) st 

(* greater-than or equal test *)
let prim_ge st =
  let ge x y = (compare_cell x y) >= 0 in
  (prim_cmp_op ge) st

(* binary math operation, chooses int or float op *)
let prim_binary_op i f st =
  let op x y =
    match x,y with
        (Int a,Int b) -> Num (Int (i a b))
      | (Int a,Float b) -> Num (Float (f (float_of_int a) b))
      | (Float a,Int b) -> Num (Float (f a (float_of_int b)))
      | (Float a,Float b) -> Num (Float (f a b))
  in
  (prim_binary_num_op op) st

(* unary math operation, choose int or float op *)
let prim_unary_op i f st =
  let op = function
    | Int x -> Num (Int (i x))
    | Float x -> Num (Float (f x))
  in
  (prim_unary_num_op op) st

(* simple numeric operations *)
let prim_plus st = (prim_binary_op ( + ) ( +. )) st 
let prim_minus st = (prim_binary_op ( - ) ( -. )) st 
let prim_times st = (prim_binary_op ( * ) ( *. )) st 
let prim_divide st = (prim_binary_op ( / ) ( /. )) st 
let prim_mod st = (prim_binary_op ( mod ) mod_float) st 
let prim_abs st = (prim_unary_op abs abs_float) st 
let prim_neg st = (prim_unary_op ( ~- ) ( ~-. )) st 

(* integer-only and float-only binary operations *)
let prim_div st = (prim_binary_int_op ( / )) st 
let prim_pow st = (prim_binary_float_op ( ** )) st 

(* floating-point operations *)
let prim_sqrt st = (prim_unary_float_op sqrt) st 
let prim_exp st = (prim_unary_float_op exp) st 
let prim_log st = (prim_unary_float_op log) st 
let prim_log10 st = (prim_unary_float_op log10) st 
let prim_sin st = (prim_unary_float_op sin) st 
let prim_cos st = (prim_unary_float_op cos) st 
let prim_tan st = (prim_unary_float_op tan) st 
let prim_asin st = (prim_unary_float_op asin) st 
let prim_acos st = (prim_unary_float_op acos) st 
let prim_atan st = (prim_unary_float_op atan) st 
let prim_sinh st = (prim_unary_float_op sinh) st 
let prim_cosh st = (prim_unary_float_op cosh) st 
let prim_tanh st = (prim_unary_float_op tanh) st 
let prim_ceil st = (prim_unary_float_op ceil) st 
let prim_floor st = (prim_unary_float_op floor) st 

(* integer fast ops *)
let prim_inc st = (prim_unary_int_op (fun n -> n + 1)) st
let prim_dec st = (prim_unary_int_op (fun n -> n - 1)) st
let prim_lshift st = (prim_unary_int_op (fun n -> n lsl 1)) st
let prim_rshift st = (prim_unary_int_op (fun n -> n asr 1)) st

(* bitwise operations *)
let prim_lnot st = (prim_unary_int_op ( lnot )) st 
let prim_land st = (prim_binary_int_op ( land )) st 
let prim_lor st = (prim_binary_int_op ( lor )) st 
let prim_lxor st = (prim_binary_int_op ( lxor )) st 
let prim_lsl st = (prim_binary_int_op ( lsl )) st 
let prim_lsr st = (prim_binary_int_op ( lsr )) st 
let prim_asr st = (prim_binary_int_op ( asr )) st 

(* truncate a float *)
let prim_truncate st =
  let trunc = function
    | Float f -> int_of_float f
    | Int i -> i
  in
  match st.stack with
      x::xs -> { st with stack=Num (Int (trunc (num_of_cell x)))::xs }
    | _ -> raise Stack_underflow

(* coerce an integer to a float *)
let prim_float st =
  let float = function
    | Float f -> f
    | Int i -> float_of_int i
  in
  match st.stack with
      x::xs -> { st with stack=Num (Float (float (num_of_cell x)))::xs }
    | _ -> raise Stack_underflow

(* test for zero, compares very close as well *)
let prim_zero flag st =
  let zerop = function
    | Int n -> (n = 0) = flag
    | Float n ->
      match classify_float n with
          FP_zero -> flag
        | FP_subnormal -> flag
        | _ -> not flag
  in
  match st.stack with
      x::xs -> { st with stack=Bool (zerop (num_of_cell x))::xs }
    | _ -> raise Stack_underflow

(* test for negative *)
let prim_negative st =
  let minusp = function
    | Float f -> f < 0.
    | Int i -> i < 0
  in
  match st.stack with
      x::xs -> { st with stack=Bool (minusp (num_of_cell x))::xs }
    | _ -> raise Stack_underflow

(* test for negative *)
let prim_positive st =
  let plusp = function
    | Float f -> f > 0.
    | Int i -> i > 0
  in
  match st.stack with
      x::xs -> { st with stack=Bool (plusp (num_of_cell x))::xs }
    | _ -> raise Stack_underflow

(* the floating-point epsilon constant (int-size specific) *)
let prim_epsilon st = push st (Num (Float epsilon_float))

(* min/max integers and floats *)
let prim_min_int st = push st (Num (Int min_int))
let prim_max_int st = push st (Num (Int max_int))
let prim_min_float st = push st (Num (Float min_float))
let prim_max_float st = push st (Num (Float max_float))

(* random number generation *)
let prim_random st = (prim_unary_op Random.int Random.float) st

(* boolean not *)
let prim_not st =
  fmap (fun x -> Bool (not (bool_of_cell x))) st

(* boolean and *)
let prim_and st =
  prim_op (fun x y -> Bool ((bool_of_cell x) && (bool_of_cell y))) st

(* boolean or *)
let prim_or st =
  prim_op (fun x y -> Bool ((bool_of_cell x) || (bool_of_cell y))) st
  
(* coerce a value to a string *)
let prim_form st =
  match st.stack with
      Str s::_ -> st
    | x::xs -> { st with stack=Str (mold x)::xs }
    | _ -> raise Stack_underflow 

(* concatenate two strings *)
let prim_strcat st =
  prim_op (fun x y -> Str ((string_of_cell x) ^ (string_of_cell y))) st

(* concatenate two strings with a space between them *)
let prim_strcat_space st =
  prim_op (fun x y -> Str (String.concat " " [ string_of_cell x
                                             ; string_of_cell y
                                             ]))
  st

(* convert a string to uppercase *)
let prim_uppercase st =
  fmap (fun x -> Str (String.uppercase (string_of_cell x))) st

(* convert a string to lowercase *)
let prim_lowercase st =
  fmap (fun x -> Str (String.lowercase (string_of_cell x))) st

(* get a substring in a string *)
let prim_strip st =
  let strip i x xs =
    let s = string_of_cell x in
    let len = String.length s in
    let n = min len (max (int_of_cell i) 0) in
    let left = String.sub s 0 n in
    let right = String.sub s n (len - n) in
    Str left::Str right::xs
  in
  match st.stack with
      i::x::xs -> { st with stack=strip i x xs }
    | _ -> raise Stack_underflow

(* extract a character in a sting *)
let prim_char st =
  let char i s = Char (string_of_cell s).[int_of_cell i] in
  match st.stack with
      i::s::xs -> { st with stack=(char i s)::xs }
    | _ -> raise Stack_underflow

(* set a character in a string *)
let prim_set_char st =
  let char c i s = 
    let dup = String.copy (string_of_cell s) in
    dup.[int_of_cell i] <- char_of_cell c;
    Str dup
  in
  match st.stack with
      i::s::c::xs -> { st with stack=(char c i s)::xs }
    | _ -> raise Stack_underflow

(* match a regex to a string *)
let prim_match st =
  let test r s = 
    Str.string_match (regex_of_cell r) (string_of_cell s) 0 
  in
  match st.stack with
      r::s::xs -> { st with stack=Bool (test r s)::xs }
    | _ -> raise Stack_underflow

(* scan forward through a string for a regex match, return pos T or F *)
let prim_scan st =
  let scan r s xs =
    try
      let i = Str.search_forward (regex_of_cell r) (string_of_cell s) 0 in
      Bool true::Num (Int i)::xs
    with Not_found -> Bool false::xs
  in
  match st.stack with
      r::s::xs -> { st with stack=scan r s xs }
    | _ -> raise Stack_underflow

(* scan backwards through a string for a regex match *)
let prim_rscan st =
  let scan r s xs =
    try
      let s = string_of_cell s in
      let n = String.length s in
      let i = Str.search_backward (regex_of_cell r) s (n - 1) in
      Bool true::Num (Int i)::xs
    with Not_found -> Bool false::xs
  in
  match st.stack with
      r::s::xs -> { st with stack=scan r s xs }
    | _ -> raise Stack_underflow

(* dump the stack to stdout *)
let prim_stack st =
  if st.cs <> [] then Terminal.show_rev_stack st.cs;
  Terminal.show_stack st.stack;
  st

(* dump the control stack to stdout *)
let prim_control_stack st =
  Terminal.show_stack st.cs;
  st

(* output a string to stdout using a given function *)
let prim_print f st =
  match st.stack with
      x::xs -> begin f (string_of_cell x); { st with stack=xs } end
    | _ -> raise Stack_underflow

(* output a character to stdout *)
let prim_princ st =
  match st.stack with
      x::xs -> (print_char (char_of_cell x); { st with stack=xs })
    | _ -> raise Stack_underflow

(* coerce a string to a filespec *)
let prim_file st =
  match st.stack with
      x::xs -> { st with stack=Filespec (File (string_of_cell x))::xs }
    | _ -> raise Stack_underflow

(* coerce a string to a url *)
let prim_url st =
  match st.stack with
      x::xs -> let url = Neturl.parse_url (string_of_cell x) in
               { st with stack=Filespec (Url url)::xs }
    | _ -> raise Stack_underflow
  
(* read a line of input from stdin *)
let prim_input st =
  try
    { st with stack=Bool true::Str (read_line ())::st.stack }
  with
      End_of_file -> { st with stack=Bool false::st.stack }
    | e -> raise e

(* read from a filespec *)
let prim_read st =
  let read = function
    | File f -> Str (file_in f)
    | Url url -> Str (http_get (Neturl.string_of_url url))
  in
  match st.stack with
      x::xs -> { st with stack=read (spec_of_cell x)::xs }
    | _ -> raise Stack_underflow
  
(* file-in source from a location *)
let prim_load st =
  let load st f = eval st (file_in f) in
  match st.stack with
      x::xs -> load { st with stack=xs } (file_of_cell x)
    | _ -> raise Stack_underflow

