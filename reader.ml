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
  ; ident_start    = letter <|> one_of op_chars <|> char '_'
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
                        ; array
                        ; block
                        ; expr
                        ; string
                        ; num
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

(* an array *)
and array st =
  let dims = char '#' >> sep_by1 decimal (char ',') in
  let make_array dim xs =
    let len = List.fold_left ( * ) 1 dim in
    let arr = Array.make len Undef in
    ignore (List.fold_left (fun i x -> arr.(i) <- x; i + 1) 0 xs);
    return (Array (dim,arr))
  in
  (dims >>= (fun dim -> braces lexer (many token) >>= make_array dim)) st

(* a block of tokens *)
and block st = (brackets lexer (many token) >>= fun xs -> return (Block xs)) st

(* a block expression *)
and expr st = (parens lexer (many1 token) >>= fun  xs -> return (Expr xs)) st

(* string literal *)
and string st = (string_lit lexer >>= fun s -> return (Str s)) st

(* numeric literal *)
and num st = 
  let real_or_nat x = not_followed_by lexer.ident_start >> return x in
  let float = real >>= fun n -> return (Float n) in
  let nat = natural >>= fun n -> return (Int n) in
  ((float <|> nat) >>= real_or_nat >>= fun x -> return (Num x)) st

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
    choose [ char ':' >> return (Word (Var s))
           ; if is_op s.Atom.name
             then return (Word (Binary (Op,s)))
             else return (Word (Sym s))
           ]
  in
  (atom >>= word) st

(* convert a string to a list of tokens *)
let tokenize s = parse s (between (whitespace lexer) eof (many token))

