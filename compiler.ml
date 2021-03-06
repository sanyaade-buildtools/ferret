(* ferret tokenizer, stream parser, and compiler
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * compiler.ml
 *)

open Lexer
open Parsec
open Atom

type lexeme =
  | Kwd of string
  | Ident of string
  | Cell of Cell.t

exception Invalid_module of string
exception Module_not_found of string
exception No_dictionary
exception Parse_error
exception Regex_parse_error of string
exception Syntax_error of string
exception Unbound_symbol of string

(* lexer for the language *)
let lexer =
  { comment_start  = string "("
  ; comment_end    = string ")"
  ; comment_line   = string "--"
  ; ident_start    = alphanum <|> one_of "_~!@$*&/+=<>?,.-;:"
  ; ident_letter   = alphanum <|> one_of "_~!@$*&/+=<>?,.-;:"
  ; op_start       = one_of "[]{}"
  ; op_letter      = pzero
  ; reserved_names = [ "in"
                     ; "use"
                     ; "previous"
                     ; "as"
                     ; "newtype"
                     ; ":"
                     ; "let"
                     ; "let:"
                     ; "->"
                     ; ";"
                     ; "inline"
                     ; "private"
                     ; "exit"
                     ; "recurse"
                     ; "if"
                     ; "else"
                     ; "then"
                     ; "begin"
                     ; "while"
                     ; "until"
                     ; "repeat"
                     ; "again"
                     ; "for"
                     ; "each"
                     ; "next"
                     ; "F"
                     ; "T"
                     ; "nan"
                     ; "inf"
                     ; "-inf"
                     ]
  ; reserved_ops   = [ "["
                     ; "]"
                     ; "{"
                     ; "}"
                     ]
  }

(* parse a single token *)
let token = 
  (* create a numeric literal lexeme *)
  let number = function
    | Left f -> return (Cell (Cell.Num (Cell.Float f)))
    | Right i -> return (Cell (Cell.Num (Cell.Int i)))
  in

  (* compile regular expression *)
  let make_re case_fold s =
    try
      if case_fold
      then return (Cell (Cell.Re (s,Str.regexp_case_fold s)))
      else return (Cell (Cell.Re (s,Str.regexp s)))
    with Failure reason -> raise (Regex_parse_error reason) 
  in

  (* parse the next token *)
  choose [ reserved lexer "in" >> return (Kwd "in")
         ; reserved lexer "use" >> return (Kwd "use")
         ; reserved lexer "previous" >> return (Kwd "previous")
         ; reserved lexer "as" >> return (Kwd "as")
         ; reserved lexer "newtype" >> return (Kwd "newtype")
         ; reserved lexer ":" >> return (Kwd ":")
         ; reserved lexer "let" >> return (Kwd "let")
         ; reserved lexer "let:" >> return (Kwd "let:")
         ; reserved lexer "->" >> return (Kwd "->")
         ; reserved lexer ";" >> return (Kwd ";")
         ; reserved lexer "inline" >> return (Kwd "inline")
         ; reserved lexer "private" >> return (Kwd "private")
         ; reserved lexer "exit" >> return (Kwd "exit")
         ; reserved lexer "recurse" >> return (Kwd "recurse")
         ; reserved lexer "if" >> return (Kwd "if")
         ; reserved lexer "else" >> return (Kwd "else")
         ; reserved lexer "then" >> return (Kwd "then")
         ; reserved lexer "begin" >> return (Kwd "begin")
         ; reserved lexer "while" >> return (Kwd "while")
         ; reserved lexer "repeat" >> return (Kwd "repeat")
         ; reserved lexer "until" >> return (Kwd "until")
         ; reserved lexer "again" >> return (Kwd "again")
         ; reserved lexer "for" >> return (Kwd "for")
         ; reserved lexer "each" >> return (Kwd "each")
         ; reserved lexer "next" >> return (Kwd "next")
         ; reserved lexer "F" >> return (Cell (Cell.Bool false))
         ; reserved lexer "T" >> return (Cell (Cell.Bool true))
         ; reserved lexer "nan" >> return (Cell (Cell.Num (Cell.Float nan)))
         ; reserved lexer "inf" >> return (Cell (Cell.Num (Cell.Float infinity)))
         ; reserved lexer "-inf" >> return (Cell (Cell.Num (Cell.Float neg_infinity)))
         ; char '#' >> reserved_op lexer "[" >> return (Kwd "#[")
         ; reserved_op lexer "[" >> return (Kwd "[")
         ; reserved_op lexer "]" >> return (Kwd "]")
         ; reserved_op lexer "{" >> return (Kwd "{")
         ; reserved_op lexer "}" >> return (Kwd "}")
         ; attempt (real_or_natural lexer) >>= number
         ; char '#' >> string_lit lexer >>= (make_re false)
         ; string_lit lexer >>= (fun s -> return (Cell (Cell.Str s)))
         ; char_lit lexer >>= (fun c -> return (Cell (Cell.Char c)))
         ; identifier lexer >>= (fun s -> return (Ident s))
         ]

(* convert a string to a list of tokens *)
let tokenize = parse (between (whitespace lexer) eof (many token))

(* validate a symbolic literal *)
let symbol s = 
  if s.[0] >= 'A' && s.[0] <= 'Z' then intern s else raise (Syntax_error s)

(* validate a word name *)
let word s =
  if s.[0] < 'A' || s.[0] > 'Z' then intern s else raise (Syntax_error s)

(* a module name from a string *)
let module_name s =
  if s.[0] < 'A' || s.[0] > 'Z' then raise (Invalid_module s) else intern s

(* shuffle a loaded module to the top *)
let shuffle st p =
  try
    let m = List.assq p st.Cell.env in
    let rm = List.remove_assq p st.Cell.env in
    { st with Cell.env=(p,m)::rm }
  with Not_found -> raise (Module_not_found p.Atom.name)

(* move a module to the top or push a new module *)
let push st p =
  let m = module_name p in
  let d = IntMap.empty in
  try shuffle st m with _ -> { st with Cell.env=(m,d)::st.Cell.env }

(* shuffle loaded modules to the top of the dictionary *)
let use st ms = 
  let st' = List.fold_left (fun st m -> shuffle st (module_name m)) st ms in
  match st.Cell.env with
      (m,_)::ms -> shuffle st' m
    | [] -> st'

(* shuffle the previous module (if any) to the top of the dictionary *)
let previous st =
  match st.Cell.env with
      a::b::xs -> { st with Cell.env=b::a::xs }
    | _ -> st

(* define a new word entry in the top dictionary *)
let define st def =
  match st.Cell.env with
      [] -> raise No_dictionary
    | (k,d)::ds ->
      { st with Cell.env=(k,IntMap.add def.Cell.word.Atom.i def d)::ds }

(* define a new colon word in the top library *)
let bind st s xs =
  let s' = word s in
  let def =
      { Cell.word=s'
      ; Cell.def=Cell.Colon xs 
      ; Cell.flags=[]
      }
  in
  define st def

(* define a constant in the top library *)
let const st s =
  let s' = word s in
  let def x =
      { Cell.word=s'
      ; Cell.def=Cell.Const x
      ; Cell.flags=[]
      }
  in
  match st.Cell.stack with  
      x::xs -> define { st with Cell.stack=xs } (def x)
    | _ -> raise Cell.Stack_underflow

(* field index primitive *)
let field i st =
  match st.Cell.stack with
      x::xs -> { st with Cell.stack=(Cell.tuple_of_cell x).(i)::xs }
    | _ -> raise Cell.Stack_underflow

(* create a tuple of a given size *)
let make_tuple i st =
  let arr = Array.make i Cell.Unit in
  let st' = ref st in
  for n = i - 1 downto 0 do
    match (!st').Cell.stack with
        x::xs -> begin arr.(n) <- x; st' := { !st' with Cell.stack=xs } end
      | _ -> raise Cell.Stack_underflow
  done;
  { !st' with Cell.stack=Cell.Tuple arr::(!st').Cell.stack }

(* define a new tuple type constructor and field accessors *)
let newtype st s ms =
  let len = List.length ms in
  let make_field (i,st) m =
    let st' = define st { Cell.word=m
                        ; Cell.def=Cell.Prim (field i)
                        ; Cell.flags=[]
                        }
    in
    (i+1,st')
  in
  let st' = snd (List.fold_left make_field (0,st) ms) in
  define st' { Cell.word=word s
             ; Cell.def=Cell.Prim (make_tuple len)
             ; Cell.flags=[]
             }

(* lookup a word in the dictionary *)
let find st ps s =
  let rec lookup = function
    | [] -> raise Not_found
    | (_,d)::ds -> try IntMap.find s.Atom.i d with Not_found -> lookup ds
  in
  try
    if List.mem s ps
    then Cell.Local s
    else Cell.Word (lookup st.Cell.env)
  with Not_found -> raise (Unbound_symbol s.Atom.name)

(* parse a program of bindings and executable tokens *)
let rec prog st = parser
  | [< 'Kwd "in"; 'Ident s; xs,st'=prog (push st s) >] -> xs,st'
  | [< 'Kwd "use"; ms=modules; xs,st'=prog (use st ms) >] -> xs,st'
  | [< 'Kwd "previous"; xs,st'=prog (previous st) >] -> xs,st'
  | [< 'Kwd "as"; 'Ident s; xs,st'=prog (const st s) >] -> xs,st'
  | [< 'Kwd "newtype"; 'Ident s; ms=fields; xs,st'=prog (newtype st s ms) >] -> xs,st'
  | [< 'Kwd ":"; 'Ident s; xs=body st []; xs,st'=prog (bind st s xs) >] -> xs,st'
  | [< 'Kwd "let:"; 'Ident s; xs=flet st []; ys=body (bind st s xs) []; xs',st'=prog st >] ->
    Cell.With ([],ys)::xs',st'
  | [< 'Kwd "let"; ps=locals; xs=body st ps; xs',st'=prog st >] -> 
    Cell.With (ps,xs)::xs',st'
  | [< x=factor st []; xs,st'=prog st >] -> x::xs,st'
  | [< 'Kwd s >] -> raise (Syntax_error s)
  | [< >] -> [],st

(* module list *)
and modules = parser
  | [< 'Ident x; xs=modules >] -> x::xs
  | [< 'Kwd ";" >] -> []
  | [< >] -> []

(* newtype record member fields *)
and fields = parser
  | [< 'Ident x; xs=fields >] -> (word x)::xs
  | [< 'Kwd ";" >] -> []
  | [< >] -> []

(* lexical frame *)
and body st ps = parser
  | [< 'Kwd ";" >] -> []
  | [< 'Kwd "let:"; 'Ident s; xs=flet st ps; ys=body (bind st s xs) ps >] -> 
    [Cell.With ([],ys)]
  | [< 'Kwd "let"; ps'=locals; xs=body st (ps' @ ps) >] -> [Cell.With (ps',xs)]
  | [< x=factor st ps; xs=body st ps >] -> x::xs
  | [< >] -> []

(* lambda function *)
and flet st ps = parser
  | [< 'Kwd "->" >] -> []
  | [< 'Kwd "let:"; 'Ident s; xs=flet st ps; ys=body (bind st s xs) ps >] -> 
    [Cell.With ([],ys)]
  | [< 'Kwd "let"; ps'=locals; xs=body st (ps' @ ps) >] -> [Cell.With (ps',xs)]
  | [< x=factor st ps; xs=flet st ps >] -> x::xs
  | [< >] -> []

(* local bindings *)
and locals = parser
  | [< 'Kwd "->" >] -> []
  | [< 'Ident p; ps=locals >] -> ps @ [intern p]

(* non-top-level factor *)
and factor st ps = parser
  | [< 'Kwd "exit" >] -> Cell.Exit
  | [< 'Kwd "recurse" >] -> Cell.Recurse
  | [< 'Kwd "["; xs=expr st ps >] -> Cell.ListExpr xs
  | [< 'Kwd "#["; xs=expr st ps >] -> Cell.TupleExpr xs
  | [< xt=branch st ps >] -> xt
  | [< xt=xt st ps >] -> xt

(* conditionals and loops *)
and branch st ps = parser
  | [< 'Kwd "if"; (ts,es)=if_else_then st ps >] -> Cell.If (ts,es)
  | [< 'Kwd "for"; xs=for_loop st ps >] -> Cell.For xs
  | [< 'Kwd "each"; xs=for_loop st ps >] -> Cell.Each xs
  | [< 'Kwd "begin"; xt=begin_loop st ps [] >] -> xt

(* if .. else/then *)
and if_else_then st ps = parser
  | [< 'Kwd "else"; es=else_then st ps >] -> ([],es)
  | [< 'Kwd "then" >] -> ([],[])
  | [< x=factor st ps; (ts,es)=if_else_then st ps >] -> (x::ts,es)

(* else .. then *)
and else_then st ps = parser
  | [< 'Kwd "then" >] -> []
  | [< x=factor st ps; xs=else_then st ps >] -> x::xs

(* for/each .. next *)
and for_loop st ps = parser
  | [< 'Kwd "next" >] -> []
  | [< x=factor st ps; xs=for_loop st ps >] -> x::xs

(* begin .. again/until/while *)
and begin_loop st ps xs = parser
  | [< 'Kwd "again" >] -> Cell.Loop xs
  | [< 'Kwd "until" >] -> Cell.Until xs
  | [< 'Kwd "while"; es=while_repeat st ps >] -> Cell.While (xs,es)
  | [< x=factor st ps; xt=begin_loop st ps (xs @ [x])>] -> xt

(* while .. repeat *)
and while_repeat st ps = parser
  | [< 'Kwd "repeat" >] -> []
  | [< x=factor st ps; xs=while_repeat st ps >] -> x::xs

(* words and literals *)
and xt st ps = parser
  | [< 'Ident s >] -> if s.[0] >= 'A' && s.[0] <= 'Z' 
                      then Cell.Lit (Cell.Atom (intern s))
                      else find st ps (intern s)
  | [< x=literal st ps >] -> Cell.Lit (x)

(* literal constant *)
and literal st ps = parser
  | [< 'Cell x >] -> x
  (*| [< 'Kwd "#["; xs=list st ps >] -> Cell.List xs*)
  | [< 'Kwd "{"; xs=block st ps >] -> Cell.Block ([],xs)
  | [< 'Ident s >] -> Cell.Atom (symbol s)

(* list expression *)
and expr st ps = parser
  | [< 'Kwd "]" >] -> []
  | [< x=factor st ps; xs=expr st ps >] -> x::xs

(* literal list *)
and list st ps = parser
  | [< 'Kwd "]" >] -> []
  | [< x=literal st ps; xs=list st ps >] -> x::xs

(* block literal *)
and block st ps = parser
  | [< 'Kwd "}" >] -> []
  | [< x=factor st ps; xs=block st ps >] -> x::xs

(* compile a string and create a list of executable tokens *)
let compile st s =
  match tokenize s with
      Some (tokens,_) -> prog st (Stream.of_list tokens)
    | None -> raise Parse_error

