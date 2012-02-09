" Vim syntax file
" Language: Ferret
" Maintainer: Jeffrey Massung
" Filenames: *.ferret
" Latest Revision: 1 Jan, 2012

if exists("b:current_syntax")
  finish
endif

" Keyword chacters
set isk=_,!,@,&,*,=,+,<,>,?,\,,/,-

" Case-insensitive
syn case ignore

" Comments
syn match ferretComment ";.*$"

" Strings and characters
syn region ferretString oneline start=+"+ skip=+\"+ end=+"+ contains=ferretChar

" Characters
syn match  ferretChar contained "\^[^[:space:][]"

" Words
syn match ferretWord    "\a\k*"
syn match ferretVar     "\a\k*:"
syn match ferretTopVar  "^\a\k*:"
syn match ferretQuote   ":\a\k*"

" Infix operators
syn match ferretInfix   "`\a\k*`"

" Numbers
syn match ferretInt     "[+-]\=\d\+"
syn match ferretNumber  "[+-]\=\(\d\+\('\d*\)*\)\=[.]\d*\(e[+-]\=\d\+\)\="
syn match ferretHex     "[+-]\=0x\x\+"
syn match ferretOct     "[+-]\=0o\o\+"

" Keywords
syn keyword ferretConst none stdout stdin stderr object
syn keyword ferretConst true false 
syn keyword ferretConst newline space cr lf tab
syn keyword ferretKey   fn let return make
syn keyword ferretKey   if when unless
syn keyword ferretKey   for foreach forever while until

" Define highlighting
hi def link ferretNumber  Number
hi def link ferretInt     Number
hi def link ferretHex     Number
hi def link ferretOct     Number
hi def link ferretComment Comment
hi def link ferretKey     Keyword
hi def link ferretString  String
hi def link ferretVar     Identifier
hi def link ferretTopVar  Special
hi def link ferretConst   Constant
hi def link ferretWord    Normal
hi def link ferretQuote   Special
hi def link ferretInfix   Type
hi def link ferretChar    SpecialChar

let b:current_syntax="ferret"
