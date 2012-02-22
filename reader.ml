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

(* compiled parse token *)
type tok =
  | Let of Atom.t * Cell.xt list
  | XT of Cell.xt

(* lexer for the language *)
let lexer =
  { comment_start  = string "("
  ; comment_end    = string ")"
  ; comment_line   = string "--"
  ; ident_start    = letter <|> one_of "!@*&/+=<>?,.-;"
  ; ident_letter   = alphanum <|> one_of "!@*&/+=<>?,.-;"
  ; op_start       = one_of "[]{}"
  ; op_letter      = pzero
  ; reserved_names = [ "let"
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
  choose [ reserved lexer "let" >> return (Kwd "let")
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

(* parse a program of bindings and executable tokens *)
let rec prog = parser
  | [< 'Kwd "let"; 'Ident s; xs=body; bs=prog >] -> Let (intern s,xs)::bs
  | [< 'Kwd "with"; ps=locals; xs=body; bs=prog >] -> XT (Cell.With (ps,xs))::bs
  | [< x=factor; xs=prog >] -> XT x::xs
  | [< 'Kwd s >] -> raise (Syntax_error s)
  | [< >] -> []

(* lexical frame *)
and body = parser
  | [< 'Kwd ";" >] -> []
  | [< 'Kwd "with"; ps=locals; xs=body >] -> [Cell.With (ps,xs)]
  | [< x=factor; xs=body >] -> x::xs
  | [< >] -> []

(* local bindings *)
and locals = parser
  | [< 'Kwd "->" >] -> []
  | [< 'Ident p; ps=locals >] -> ps @ [intern p]

(* body factor *)
and factor = parser
  | [< 'Kwd "exit" >] -> Cell.Exit
  | [< xt=branch >] -> xt
  | [< xt=xt >] -> xt

(* conditionals and loops *)
and branch = parser
  | [< 'Kwd "if"; (ts,es)=if_else_then >] -> Cell.If (ts,es)
  | [< 'Kwd "for"; xs=for_loop >] -> Cell.For xs
  | [< 'Kwd "each"; xs=for_loop >] -> Cell.Each xs
  | [< 'Kwd "begin"; xt=begin_loop [] >] -> xt

(* if .. else/then *)
and if_else_then = parser
  | [< 'Kwd "else"; es=else_then >] -> ([],es)
  | [< 'Kwd "then" >] -> ([],[])
  | [< x=factor; (ts,es)=if_else_then >] -> (x::ts,es)

(* else .. then *)
and else_then = parser
  | [< 'Kwd "then" >] -> []
  | [< x=factor; xs=else_then >] -> x::xs

(* for/each .. next *)
and for_loop = parser
  | [< 'Kwd "next" >] -> []
  | [< x=factor; xs=for_loop >] -> x::xs

(* begin .. again/until/while *)
and begin_loop xs = parser
  | [< 'Kwd "again" >] -> Cell.Loop xs
  | [< 'Kwd "until" >] -> Cell.Until xs
  | [< 'Kwd "while"; es=while_repeat >] -> Cell.While (xs,es)
  | [< x=factor; xt=begin_loop (xs @ [x])>] -> xt

(* while .. repeat *)
and while_repeat = parser
  | [< 'Kwd "repeat" >] -> []
  | [< x=factor; xs=while_repeat >] -> x::xs

(* words and literals *)
and xt = parser
  | [< 'Ident s >] -> Cell.Word (intern s)
  | [< x=literal >] -> Cell.Lit (x)

(* literal constant *)
and literal = parser
  | [< 'Kwd "true" >] -> Cell.Bool true
  | [< 'Kwd "false" >] -> Cell.Bool false
  | [< 'Kwd "["; xs=list >] -> Cell.List xs
  | [< 'Kwd "{"; xs=block >] -> Cell.Block ([],xs)
  | [< 'Float f >] -> Cell.Num (Cell.Float f)
  | [< 'Int i >] -> Cell.Num (Cell.Int i)
  | [< 'String s >] -> Cell.Str s
  | [< 'Char c >] -> Cell.Char c

(* list literal *)
and list = parser
  | [< 'Kwd "]" >] -> []
  | [< x=literal; xs=list >] -> x::xs

(* block literal *)
and block = parser
  | [< 'Kwd "}" >] -> []
  | [< x=factor; xs=block >] -> x::xs

(* compile a string and create a list of executable tokens *)
let compile st s =
  match tokenize s with
      Some (tokens,_) -> prog (Stream.of_list tokens)
    | None -> raise Parse_error

