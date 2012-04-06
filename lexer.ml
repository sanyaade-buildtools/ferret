(* parsec-like lexer for ocaml
 *
 * copyright (c) 2011 jeffrey massung
 * all rights reserved
 *
 * lexer.ml
 *)

open Parsec

(* simple union type *)
type ('a,'b) either = Left of 'a | Right of 'b

(* function that accepts a parser and returns a value *)
type 'a parse_combinator = parse_stream option -> 'a parse_state

(* language lexer definition *)
type lexer = { comment_start  : string parse_combinator
             ; comment_end    : string parse_combinator
             ; comment_line   : string parse_combinator
             ; ident_start    : char parse_combinator
             ; ident_letter   : char parse_combinator
             ; op_start       : char parse_combinator
             ; op_letter      : char parse_combinator
             ; reserved_names : string list
             ; reserved_ops   : string list
             }

(* dummy lexer to build off of *)
let dummy =
  { comment_start = string "/*"
  ; comment_end = string "*/"
  ; comment_line = string "//"
  ; ident_start = letter <|> char '_'
  ; ident_letter = alphanum <|> char '_'
  ; op_start = one_of "~!%^&*-=+"
  ; op_letter = one_of "~!%^&*-=+"
  ; reserved_names = []
  ; reserved_ops = []
  }

(* skip a single-line comment *)
let single_line_comment l =
  l.comment_line >> (many_till any_char eol)

(* skip a multi-line comment *)
let block_comment l =
  l.comment_start >> (many_till any_char l.comment_end)

(* skip any comment *)
let comment l =
  (single_line_comment l) <|> (block_comment l)

(* skip all whitspace and comments *)
let whitespace l =
  skip ((skip1 (comment l)) <|> (skip1 (space <|> newline)))

(* parse and skip whitespace *)
let lexeme l p =
  p >>= (fun x -> maybe (whitespace l) >> return x)

(* parse an identifier *)
let identifier l =
  let id x = if List.mem x l.reserved_names then pzero else return x in
  lexeme l (capture (l.ident_start >> many l.ident_letter)) >>= id

(* parse an operator *)
let operator l =
  let op x = if List.mem x l.reserved_ops then pzero else return x in
  lexeme l (capture (l.op_start >> many l.op_letter)) >>= op

(* parse a reserved identifier *)
let reserved l s =
  let id x = if x = s then return x else pzero in
  lexeme l (capture (l.ident_start >> many l.ident_letter)) >>= id

(* parse a reserved operator *)
let reserved_op l s =
  let op x = if x = s then return x else pzero in
  lexeme l (capture (l.op_start >> many l.op_letter)) >>= op

(* parse an escaped character *)
let escaped_char =
  let esc = function
    | 't' -> return '\t'
    | 'r' -> return '\r'
    | 'n' -> return '\n'
    | 'b' -> return '\b'
    | c   -> return c
  in (char '\\' >> any_char >>= esc) <|> any_char

(* parse a character literal *)
let char_lit l =
  lexeme l (between (char '\'') (char '\'') escaped_char)

(* parse a string literal *)
let string_lit l =
  lexeme l (char '"' >> (collect (many_till escaped_char (char '"'))))

(* optional sign flag *)
let sign = maybe (one_of "-+")

(* int_of_string, skipping leading + if present *)
let natural_of_string d =
  if d.[0] = '+' 
  then int_of_string (String.sub d 1 (String.length d -1))
  else int_of_string d

(* utility functions *)
let integer p = capture (sign >> p) >>= fun d -> return (natural_of_string d) 
let float p = capture (sign >> p) >>= fun d -> return (float_of_string d)

(* parse an unsigned decimal number *)
let decimal = integer digits

(* parse an unsigned hex number *)
let hex = integer (string "0x" >> hex_digits)

(* parse a binary number *)
let binary = integer (string "0b" >> many1 (one_of "01"))

(* parse an octal number *)
let octal = integer (string "0o" >> many1 (one_of "01234567"))

(* parse an unsigned natural number *)
let natural = choose [hex;octal;binary;decimal]

(* parse a real number *)
let real =
  let frac = char '.' >> many digit in
  let exp = one_of "eE" >> sign >> digits in
  float (digits >> frac >> maybe exp)

(* parse a real or natural *)
let real_or_natural l =
  let num = choose [ real >>= (fun f -> return (Left f))
                   ; natural >>= (fun i -> return (Right i))
                   ]
  in
  lexeme l (num >>= fun x -> not_followed_by l.ident_start >> return x)

(* parse combinator between two guarded combinators *)
let guarded l gopen gclose p = 
  lexeme l (between (lexeme l gopen) gclose (lexeme l p))

(* parse a combinator between various guards *)
let parens l p = guarded l (char '(') (char ')') p
let brackets l p = guarded l (char '[') (char ']') p
let braces l p = guarded l (char '{') (char '}') p
let angles l p = guarded l (char '<') (char '>') p
