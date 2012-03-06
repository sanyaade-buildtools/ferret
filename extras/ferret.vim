" Vim syntax file
" Language: Ferret
" Maintainer: Jeffrey Massung
" Filenames: *.ferret
" Latest Revision: 1 Jan, 2012

if exists("b:current_syntax")
  finish
endif

" Keyword chacters
set isk=_,~,!,@,*,&,/,+,=,<,>,?,\,,\.,-,;,:,

" Case-insensitive
"syn case ignore

" Words
syn match ferretWord    "\(\d\|\a\|\k\)\+"
syn match ferretAtom    "[A-Z]\(\d\|\a\|\k\)*"

" Comments
syn match ferretComment "--.*$"

" Block comments, strings and characters
syn region ferretString start=+"+ skip=+\\"+ end=+"+ contains=ferretSpecialChar
syn region ferretChar start=+'+ skip=+\\'+ end=+'+
syn region ferretStack start=+(+ end=+)+

" Characters
syn match  ferretSpecialChar contained "\^[^[:space:][]"

" Numbers
syn match ferretInt     "[+-]\=\d\+\(\d\|\a\|\k\)\@!"
syn match ferretNumber  "[+-]\=\d\+[.]\d*\(e[+-]\=\d\+\)\=\(\d\|\a\|\k\)\@!"
syn match ferretHex     "[+-]\=0x\x\+\(\d\|\a\|\k\)\@!"
syn match ferretOct     "[+-]\=0o\o\+\(\d\|\a\|\k\)\@!"
syn match ferretBin     "[+-]\=0b[01]\+\(\d\|\a\|\k\)\@!"

" Keywords
syn keyword ferretConst nan inf -inf
syn keyword ferretTop   in use previous as : ; 
syn keyword ferretKey   let let: ->
syn keyword ferretKey   if else then begin while repeat until again
syn keyword ferretKey   for each next 
syn keyword ferretKey   exit

" Define highlighting
hi def link ferretNumber      Number
hi def link ferretInt         Number
hi def link ferretHex         Number
hi def link ferretOct         Number
hi def link ferretBin         Number
hi def link ferretComment     Comment
hi def link ferretKey         Keyword
hi def link ferretString      String
hi def link ferretVar         Identifier
hi def link ferretTop         Special
hi def link ferretConst       Constant
hi def link ferretAtom        Constant
hi def link ferretWord        Normal
hi def link ferretQuote       Special
hi def link ferretStack       Type
hi def link ferretChar        SpecialChar
hi def link ferretSpecialChar SpecialChar

let b:current_syntax="ferret"
