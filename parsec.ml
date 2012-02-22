(* parsec-like parser for ocaml
 *
 * copyright (c) 2011 jeffrey massung
 * all rights reserved
 *
 * parsec.ml
 *)

(* input string and position *)
type parse_stream = { source : string
                    ; pos : int
                    ; line : int
                    }

(* current parser result and input stream *)
type 'a parse_state = ('a * parse_stream) option

(* parse failure *)
exception Fail of parse_stream

(* bind from one parse combinator to the next *)
let (>>=) m f st =
  match m st with
      Some (x,st') -> (f x) (Some st')
    | None -> None

(* bind two combinators, ignore the intermediate result *)
let (>>) m p st =
  match m st with
      Some (_,st') -> p (Some st')
    | None -> None

(* terminate a bind chain and return *)
let return x = function
  | Some st -> Some (x,st)
  | None -> None

(* attempt to parse a source string with a parse combinator *)
let parse p s =
  try
    match p (Some { source=s; pos=0; line=1 }) with
        Some (x,st') -> Some (x,st')
      | None -> None
  with Fail st -> None

(* force a parse to fail *)
let pzero _ = None

(* fails with an exception - no more parsing is done *)
let fail = function
  | Some st -> raise (Fail st)
  | None -> None

(* match any character except eof *)
let any_char = function
  | None -> None
  | Some st ->
    try
      let c = st.source.[st.pos] in
      if c = '\n'
      then Some (c,{ st with pos=st.pos+1; line=st.line+1 })
      else Some (c,{ st with pos=st.pos+1 })
    with _ -> None

(* match the end of the stream *)
let eof = function
  | None -> None
  | Some st ->
    try
      ignore st.source.[st.pos];
      None
    with _ -> Some (None,st)

(* try one combinator then another if it failed *)
let (<|>) a b st =
  match a st with
      None -> b st
    | x -> x

(* try one of many combinators, picj the first that succeeds *)
let choose ps st =
  let rec attempt st = function
    | [] -> None
    | (p::ps) ->
      match p st with
          None -> attempt st ps
        | x -> x
  in
  attempt st ps

(* match a specific character *)
let char c st =
  match any_char st with
      None -> None
    | Some (x,st') -> if x = c then Some (c,st') else None

(* match a character in a set of characters *)
let one_of cs st =
  match any_char st with
      None -> None
    | Some (c,st') ->
      try
        ignore (String.index cs c);
        Some (c,st')
      with _ -> None

(* match any character other than one from the set *)
let none_of cs st =
  match any_char st with
      None -> None
    | Some (c,st') ->
      try
        ignore (String.index cs c);
        None
      with _ -> Some (c,st')

(* match a series of characters in order *)
let string s = function
  | None -> None
  | Some st ->
    try
      let len = String.length s in
      let cs = String.sub st.source st.pos len in
      if cs = s then Some (s,{ st with pos=st.pos+len }) else None
    with _ -> None

(* optionally parse a combinator or return a default value *)
let option x p = p <|> (return x)

(* capture the text from a matched combinator into a string *)
let capture p = function
  | None -> None
  | Some st ->
    let pos' = st.pos in
    match p (Some st) with
        None -> None
      | Some (_,st') -> Some (String.sub st.source pos' (st'.pos-pos'),st')

(* collect a list of characters and make them a string *)
let collect p =
  let rec string_of_list = function
    | [] -> ""
    | c::cs -> String.make 1 c ^ string_of_list cs
  in
  p >>= fun cs -> return (string_of_list cs)

(* match a combinator one or more times *)
let rec many1 p st =
  (p >>= (fun x -> many p >>= (fun xs -> return (x::xs)))) st

(* match a combinator zero or more times *)
and many p st = ((many1 p) <|> (return [])) st

(* ignore a combinator if present *)
let maybe p = option None (p >> return None)

(* skip a combinator one or more times *)
let rec skip1 p st = (p >> (skip p) >> (return None)) st

(* skip a combinator zero or more times *)
and skip p st = ((skip1 p) <|> (return None)) st

(* capture a combinator that is between two others *)
let between start term p =
  start >> (p >>= fun x -> (term >> return x))

(* prepends current parse results to p *)
let cons p = fun x -> p >>= (fun xs -> return (x::xs))

(* captures p separated by sep one or more times *)
let rec sep_by1 p sep = p >>= (cons (many (sep >> p)))

(* captures p separated by sep zero or more times *)
and sep_by p sep = (sep_by1 p sep) <|> (return [])

(* captures p separated by - and optionally ended with - sep *)
let rec sep_end_by1 p sep =
  let rest x = sep >> ((sep_end_by p sep) >>= (fun xs -> return (x::xs)))
  in p >>= (fun x -> (rest x) <|> return [x])

(* captures p separated by sep zero or more times, optionally ending with *)
and sep_end_by p sep = (sep_end_by1 p sep) <|> (return [])

(* keep capturing p until term is found *)
let rec many_till p term st =
  ((term >> return []) <|> (p >>= cons (many_till p term))) st

(* make sure the next token isn't something specific *)
let not_followed_by p = (p >> fail) <|> return ()

(* common combinators *)
let upper_letter = one_of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let lower_letter = one_of "abcdefghijklmnopqrstuvwxyz"
let letter = upper_letter <|> lower_letter
let letters = many1 letter
let digit = one_of "0123456789"
let digits = many1 digit
let hex_digit = digit <|> one_of "abcdefABCDEF"
let hex_digits = many1 hex_digit
let oct_digit = one_of "01234567"
let oct_digits = many1 oct_digit
let alphanum = letter <|> digit
let punctuation = one_of "!@#$%^&*()-=+[]{}\\|;:'\",./<>?~`"
let space = one_of " \t"
let spaces = skip1 space
let newline = one_of "\r\n"
let newlines = skip1 newline
let eol = eof <|> (newline >> return None)
