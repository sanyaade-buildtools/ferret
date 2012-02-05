(* ferret stream reader
 *
 * copyright (c) 2012 by jeffrey massung
 * all rights reserved
 *
 * reader.ml
 *)

open Lexer
open Parsec
open Atom
open Word
open Cell

(* parse exceptions *)
exception Invalid_number of string
exception Parse_error

(* binary operator character set *)
let op_chars = "!@*&/+=<>?,.-"

(* lexer for the language *)
let lexer =
  { comment_start  = pzero
  ; comment_end    = pzero
  ; comment_line   = string ";"
  ; ident_start    = alphanum <|> one_of op_chars <|> char '_'
  ; ident_letter   = alphanum <|> one_of op_chars <|> char '_'
  ; op_start       = pzero
  ; op_letter      = pzero
  ; reserved_names = ["none";"false";"true";"nan";"inf";"-inf"]
  ; reserved_ops   = []
  }

(* parse a single token *)
let rec token st =
  (lexeme lexer (choose [ quote
                        ; constant
                        ; block
                        ; expr
                        ; string
                        ; character
                        ; word
                        ]))
    st

(* interned atom *)
and atom st = 
  let ident = lexer.ident_start >> (many lexer.ident_letter) in
  (capture ident >>= fun s -> return (intern s)) st

(* a quoted symbol *)
and quote st = (char '\'' >> token >>= fun x -> return (Quote x)) st

(* symbolic constants *)
and constant st =
  (choose [ reserved lexer "none" >> return Undef
          ; reserved lexer "false" >> return (Bool false)
          ; reserved lexer "true" >> return (Bool true)
          ; reserved lexer "nan" >> return (Num (Float nan))
          ; reserved lexer "inf" >> return (Num (Float infinity))
          ; reserved lexer "-inf" >> return (Num (Float neg_infinity))
          ])
    st

(* a block of tokens *)
and block st = (brackets lexer (many token) >>= fun xs -> return (Block xs)) st

(* a block expression *)
and expr st = (parens lexer (many1 token) >>= fun  xs -> return (Expr xs)) st

(* string literal *)
and string st = (string_lit lexer >>= fun s -> return (Str s)) st

(* character literal *)
and character st =
  let first s = try Char s.[0] with _ -> Char '\000' in
  (char '#' >> string_lit lexer >>= fun s -> return (first s)) st

(* parse a word *)
and word st =
  (choose [ getter
          ; infix
          ; symbol
          ])
    st

(* a getter word *)
and getter st = (char ':' >> atom >>= fun s -> return (Word (Getter s))) st

(* an infixed word *)
and infix st =
  let infixed = between (char '`') (char '`') in
  (infixed atom >>= fun s -> return (Word (Binary (Infix,s)))) st

(* a generic symbol *)
and symbol st =
  let is_op s =
    match parse s (many1 (one_of op_chars) >> eof) with
        Some (_,_) -> true
      | None -> false
  in
  let word s =
    try return (Num (Int (int_of_string s.name))) with _ ->
      try return (Num (Float (float_of_string s.name))) with _ ->
        if s.name.[0] = '.' || (s.name.[0] >= '0' && s.name.[0] <= '9')
        then raise (Invalid_number s.name)
        else choose [ char ':' >> return (Word (Var s))
                    ; if is_op s.Atom.name
                      then return (Word (Binary (Op,s)))
                      else return (Word (Sym s))
                    ]
  in
  (atom >>= word) st

(* convert a string to a list of tokens *)
let tokenize s = parse s (between (whitespace lexer) eof (many token))

