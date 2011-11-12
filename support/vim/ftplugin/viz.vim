" Vim filetype plugin
" Language:	Viz
" Maintainer:	Petter Urkedal <paurkedal@gmail.com>
" URL:		http://www.vizlang.org/

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

setlocal fo-=t fo+=crql

setlocal comments=b:#,b:##,s:{#,mb:#,e:#}
let b:match_words = '(:),{:},[:],⁽:⁾,₍:₎,⟦:⟧,⟨:⟩,⟪:⟫,⌈:⌉,⌊:⌋'

setlocal include=\\<\\(include\\\|open\\)\\>
setlocal includeexpr=substitute(v:fname,'\\.','/','g')
setlocal suffixesadd=.vz
