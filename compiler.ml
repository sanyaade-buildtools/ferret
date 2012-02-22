(* ferret tokenizer, stream parser, and compiler
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * compiler.ml
 *)

open Genlex
open Lexer
open Parsec
open Atom

exception Parse_error
exception Syntax_error of string
exception Unbound_symbol of string

(* compiled parse token *)
type tok =
  | Let of Atom.t * Cell.xt list
  | XT of Cell.xt

(* lexer for the language *)
let lexer =
  { comment_start  = string "("
  ; comment_end    = string ")"
  ; comment_line   = string "--"
  ; ident_start    = letter <|> one_of "!@*&/+=<>?,.-;:"
  ; ident_letter   = alphanum <|> one_of "!@*&/+=<>?,.-;:"
  ; op_start       = one_of "[]{}"
  ; op_letter      = pzero
  ; reserved_names = [ ":"
                     ; "with"
                     ; "->"
                     ; ";"
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
                     ; "false"
                     ; "true"
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
  choose [ reserved lexer ":" >> return (Kwd ":")
         ; reserved lexer "with" >> return (Kwd "with")
         ; reserved lexer "->" >> return (Kwd "->")
         ; reserved lexer ";" >> return (Kwd ";")
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
         ; reserved lexer "false" >> return (Kwd "false")
         ; reserved lexer "true" >> return (Kwd "true")
         ; reserved lexer "nan" >> return (Float nan)
         ; reserved lexer "inf" >> return (Float infinity)
         ; reserved lexer "-inf" >> return (Float neg_infinity)
         ; reserved_op lexer "[" >> return (Kwd "[")
         ; reserved_op lexer "]" >> return (Kwd "]")
         ; reserved_op lexer "{" >> return (Kwd "{")
         ; reserved_op lexer "}" >> return (Kwd "}")
         ; real_or_natural lexer
         ; string_lit lexer >>= (fun s -> return (String s))
         ; char_lit lexer >>= (fun c -> return (Char c))
         ; identifier lexer >>= (fun s -> return (Ident s))
         ]

(* convert a string to a list of tokens *)
let tokenize = parse (between (whitespace lexer) eof (many token))

(* define a new word in the top library *)
let bind st s xs =
  let def = 
    { Cell.def=Cell.Colon xs 
    } 
  in
  { st with Cell.env=IntMap.add (intern s).Atom.i def st.Cell.env }

(* lookup a word in the dictionary *)
let find st ps s =
  try
    if List.mem s ps
    then Cell.Local s
    else Cell.Word (s,IntMap.find s.Atom.i st.Cell.env)
  with Not_found -> raise (Unbound_symbol s.Atom.name)

(* parse a program of bindings and executable tokens *)
let rec prog st = parser
  | [< 'Kwd ":"; 'Ident s; xs=body st []; xs,st'=prog (bind st s xs) >] -> xs,st'
  | [< 'Kwd "with"; ps=locals; xs=body st ps; xs',st'=prog st >] -> 
    Cell.With (ps,xs)::xs',st'
  | [< x=factor st []; xs,st'=prog st >] -> x::xs,st'
  | [< 'Kwd s >] -> raise (Syntax_error s)
  | [< >] -> [],st

(* lexical frame *)
and body st ps = parser
  | [< 'Kwd ";" >] -> []
  | [< 'Kwd "with"; ps'=locals; xs=body st (ps' @ ps) >] -> [Cell.With (ps',xs)]
  | [< x=factor st ps; xs=body st ps >] -> x::xs
  | [< >] -> []

(* local bindings *)
and locals = parser
  | [< 'Kwd "->" >] -> []
  | [< 'Ident p; ps=locals >] -> ps @ [intern p]

(* body factor *)
and factor st ps = parser
  | [< 'Kwd "exit" >] -> Cell.Exit
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
  | [< 'Ident s >] -> find st ps (intern s)
  | [< x=literal st ps >] -> Cell.Lit (x)

(* literal constant *)
and literal st ps = parser
  | [< 'Kwd "true" >] -> Cell.Bool true
  | [< 'Kwd "false" >] -> Cell.Bool false
  | [< 'Kwd "["; xs=list st ps >] -> Cell.List xs
  | [< 'Kwd "{"; xs=block st ps >] -> Cell.Block ([],xs)
  | [< 'Float f >] -> Cell.Num (Cell.Float f)
  | [< 'Int i >] -> Cell.Num (Cell.Int i)
  | [< 'String s >] -> Cell.Str s
  | [< 'Char c >] -> Cell.Char c

(* list literal *)
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

