" Vim syntax file
" Language:	Viz
" Filenames:	*.vz
" Maintainers:	Petter Urkedal <paurkedal@gmail.com>
" URL:		http://www.vizlang.org/
"
" Variables to disable certain features:
"   viz_disable_patterns - Don't try to recognise constructors in patterns.
"   viz_disable_types    - Don't try to recognise type names from the context.

let s:verbs =
  \ ['assert', 'be', 'fail', 'do', 'raise']
let s:nonpattern_conditionals =
  \ ['if', 'then', 'else', 'otherwise', 'when', 'for', 'while']
let s:pattern_conditionals =
  \ ['at']
let s:declarators =
  \ ['in', 'inj', 'include', 'let', 'lex', 'open', 'sig', 'sealed',
  \  'type', 'use', 'val']
let s:connectives =
  \ ['with', 'where', 'what', 'which']

let s:conditionals = s:nonpattern_conditionals + s:pattern_conditionals
let s:keywords = s:verbs + s:conditionals + s:declarators + s:connectives
let s:keyword_re = '\<\('.join(s:keywords, '\|').'\)\>\|\.at\>'

" A variant of the below, accepting a regular expression in place of the list
" of keywords.
fun! s:delimit_clause_re(group, keyword_re, contains)
  exe 'syn region' a:group 'transparent keepend matchgroup='.a:group
    \ 'start="'.a:keyword_re.'"'
    \ 'skip="#[!#? \t].*"'
    \ 'end="\('.s:keyword_re.'\)\@="'
    \ 'contains='.a:contains
endfun

" Highlight the body of the list of clause introducers a:keywords as
" a:contains.  Make sure a:contains includes comments and, if needed, strings.
" The extend-flag on these ensures we don't terminate on a keyword buried in a
" string or a comment.
fun! s:delimit_clauses(group, keywords, contains)
  call s:delimit_clause_re(a:group, '\<\('.join(a:keywords, '\|').'\)\>',
     \                     a:contains)
endfun

set iskeyword=@,',`,48-57,_,192-255
syn case match

syn cluster VizCommon contains=@VizKeyword,@VizLiteral,
  \ VizComment,VizCommentLine,VizLabel,
  \ @VizModuleCommon,@VizOperator
syn cluster VizValExpr contains=@VizCommon,VizTyping

" Signature and Structure Prefixes
"
syn cluster VizModuleCommon
  \ contains=VizSctPrefix,VizSigPrefix,VizSctPath,VizSigPath
syn match VizSctPrefix '\K\k*\(\.\(\K\|(\)\)\@='
syn match VizSigPrefix '\k\k*\(\.\[\)\@='
syn region VizSctPath matchgroup=VizPathOperator start='\.(' end=')'
  \ fold transparent contains=VizSctExprStart
syn region VizSigPath matchgroup=VizPathOperator start='\.\[' end='\]'
  \ fold transparent contains=@VizTypeExpr
hi link VizSigPrefix VizSigName
hi link VizSctPrefix VizSctName

" Structure Expressions
"
syn cluster VizSctExpr contains=VizSctExprStart,VizSctTyping
syn match VizSctExprStart contained '\K\k*'
  \ skipnl skipwhite nextgroup=VizSctExprCont
syn match VizSctExprCont contained '\.\(at\>\)\@!\K\k*'
  \ skipnl skipwhite nextgroup=VizSctExprCont
syn region VizSctExprCont contained fold transparent matchgroup=VizPathOperator
  \ start='\.(' end=')' contains=@VizSctExpr
  \ skipnl skipwhite nextgroup=VizSctExprCont
syn region VizSctTyping contained
  \ matchgroup=VizPathOperator start='\s\@<=:\s\@=' end=')\@='
  \ contains=@VizSigExpr,VizSctPath,VizSctPrefix
hi link VizSctExprStart VizSctName
hi link VizSctExprCont VizSctName

" Signature Expressions
"
syn cluster VizSigExpr contains=VizSigName,VizSigProduct,VizSigPath,
  \ VizSigPrefix,VizConnective
syn match VizSigName contained '\.\?\K\k*\>\(\.\S\)\@!'
  \ skipwhite nextgroup=VizTypeParam
syn region VizSigProduct transparent fold keepend extend
  \ matchgroup=VizPathOperator start='\<\(Fun\|Pi\)\>' end='\.\S\@!'
  \ contains=@VizSctExpr,VizSigProduct
  \ skipwhite skipnl nextgroup=@VizSigExpr

" Types
"
syn cluster VizTypeExpr
    \ contains=VizTypeParam,VizTypeExpr,VizOperator,@VizCommon,VizTypeName
syn match VizTypeParam '\<\'\K\k*' skipwhite nextgroup=VizTypeParam
syn match VizTypeParam '[α-ω]\k*' skipwhite nextgroup=VizTypeParam
syn match VizTypeName contained '\.\?\K\k*\>\(\.\S\)\@!'
syn region VizTypeExpr transparent fold
    \ matchgroup=VizOperator start='[∀∃]\|\<[AE]`' end='\.\S\@!' keepend extend
    \ contains=@VizTypeExpr
hi link VizTypeParam vizTypeName

" Operators
"
syn cluster VizOperator contains=VizOperator,VizParen,VizOperatorName
syn match VizOperator
  \ '[-!#&*+,/;<=>@|~¬×\u2190-\u21ff∁∂\u2206-\u22ff\u2a00-\u2aff]\+\'*'
syn match VizOperator '\k\@<!:\k\@!'
syn match VizOperator '\.\S\@=\k\@!'
syn region VizParen matchgroup=VizOperator start='(' end=')' fold transparent
syn region VizParen matchgroup=VizOperator start='\[' end='\]' fold transparent
if !exists('viz_disable_types')
  syn region VizTyping contained containedin=VizParen transparent
    \ matchgroup=VizOperator start='\S\@<!:\S\@!' end=')\@='
    \ contains=@VizTypeExpr
endif
syn match VizOperatorName '\<[0-2]\'[^ \t()\[\]{}]\+'

" Patterns and Constructor Names
"
if !exists("viz_disable_patterns")
  syn cluster VizPattern contains=VizInjNameT,VizInjName,VizInjParen,@VizCommon
  syn cluster VizPatternCont contains=@VizPattern,VizInjParam
  exe 'syn match VizInjName contained'
    \ '"\K\k*\(\s\+\((\|\('.s:keyword_re.'\)\@!\K\)\@=\)"'
    \ 'skipwhite nextgroup=@VizPatternCont'
  syn match VizInjParam contained '\K\k*'
    \ skipwhite nextgroup=@VizPatternCont
  syn region VizInjParen contained fold transparent
    \ matchgroup=VizOperator start='(' end=')' contains=@VizPattern
  syn match VizInjNameT contained '\K\k*%'
    \ skipwhite nextgroup=@VizPatternCont
  hi link VizInjNameT VizInjName
endif
syn keyword VizInjName true false

" Labels
"
syn match VizLabel '\K\k*:'

" Keywords
"
syn cluster @VizKeyword contains=VizConditional,VizConnective,VizDeclarator,VizVerb
let s:k_let = '\<let\>!\?'
if exists("viz_disable_patterns")
  exe 'syn keyword VizConditional' join(s:conditionals)
  exe 'syn match VizDeclarator' s:k_let
else
  exe 'syn keyword VizConditional' join(s:nonpattern_conditionals)
  call s:delimit_clauses('VizConditional', s:pattern_conditionals, '@VizPattern')
  call s:delimit_clause_re('VizDeclarator', s:k_let, '@VizPatternCont')
endif
let s:k_type = '\<type\>[-+~]*\(:\K\+\)\?'
let s:k_inj_or_val = '\<\(inj\|val\)\>[-+]*\(:\K\+\)\?'
let s:k_dot_at = '\.at\>'
if exists('viz_disable_types')
  exe 'syn keyword VizDeclarator' s:k_type
  exe 'syn match VizDeclarator' s:k_inj_or_val
  exe 'syn match VizConnective' s:k_dot_at 'skipwhite nextgroup=VizSctExprStart'
else
  call s:delimit_clause_re('VizDeclarator', s:k_type, '@VizTypeExpr')
  call s:delimit_clause_re('VizDeclarator', s:k_inj_or_val, '@VizValExpr')
  call s:delimit_clause_re('VizConnective', s:k_dot_at, '@VizSctExpr')
endif
syn keyword VizConnective with where
syn match VizConnective '\<wh\(at\|ich\)\>!\?'
syn keyword VizDeclarator use
syn match VizDeclarator '\<include\>!\?'
syn match VizDeclarator '\<in\>!\?' skipwhite nextgroup=VizSctExprStart
syn keyword VizDeclarator sig sealed skipwhite nextgroup=@VizSigExpr
syn keyword VizDeclarator open nextgroup=VizOpenDecor
exe 'syn keyword VizVerb' join(s:verbs)

syn match VizOpenDecor contained '\(:c\)\?' skipwhite nextgroup=VizSctExprStart
hi link VizOpenDecor VizDeclarator

syn keyword VizDeclarator lex skipwhite nextgroup=VizPrecedence,VizLexAlias
syn match VizPrecedence contained skipwhite nextgroup=VizLexOperator
  \ '\<\([IJQR]\|A[0-9]S\?\|B[LR]\|L[0-8]\|S[0-2][IPS]\?\|S2L\)\>'
syn match VizLexOperator contained '\S\+' skipwhite nextgroup=VizLexOperator
syn keyword VizLexAlias contained alias skipwhite nextgroup=VizLexAliasOperator
syn match VizLexAliasOperator contained '\S\+' skipwhite nextgroup=VizLexAliasAs
syn match VizLexAliasAs contained '\S\+' skipwhite nextgroup=VizLexAliasOperator
hi link VizLexAlias VizDeclarator
hi link VizLexOperator VizOperator
hi link VizLexAliasOperator VizOperator

syn keyword VizPronoun that

" Literals
"
syn cluster VizLiteral contains=VizNumber,VizString
syn match VizNumber '-\?\<\d\+\(\.\d\+\)\?\>'
syn match VizNumber '-\?\<0b[01]\+\>' contains=ffBasePrefix
syn match VizNumber '-\?\<0o[0-7]\+\>' contains=ffBasePrefix
syn match VizNumber '-\?\<0x\x\+\>' contains=ffBasePrefix
syn match VizNumberBase contained '\<0[bvx]'

syn region VizString start='"' end='"' skip='\\.' extend contains=@VizString
syn cluster VizString contains=VizStringEscape
syn match VizStringEscape contained '\\.'

" Comments
"
syn region VizComment start='{#' end='#}' extend contains=VizComment,@VizComment
syn match VizCommentLine '#\([!#? \t].*\|$\)' contains=@VizComment
syn region VizCommentLine start='^\s*#.*' skip='^\s*#.*' end='^' keepend
  \ contains=@VizComment
hi link VizCommentLine VizComment
syn cluster VizComment
  \ contains=@Spell,VizCommentSectionNumber,VizCommentURL,
  \ VizCommentSpecial,VizCommentError

syn match VizCommentSectionNumber contained
  \ '\(^\s*#\s\+\)\@<=0\(\.[0-9]\+\)*[[:space:].]\s\@='
  \ nextgroup=VizCommentSectionSpacer
syn match VizCommentSectionNumber contained '\(^\s*#\s\+\)\@<=Z\.\([0-9Z]\+\.\)*\s\@='
  \ nextgroup=VizCommentSectionSpacer
syn match VizCommentSectionSpacer contained '\s\+' nextgroup=VizCommentSectionTitle
syn match VizCommentSectionTitle contained '.\+' contains=@Spell
  \ skipwhite skipnl nextgroup=VizCommentSectionTitleCont
syn match VizCommentSectionTitleCont transparent contained '^\s*#\s\+\S\@='
  \ nextgroup=VizCommentSectionTitle

syn match VizCommentURL contained '\<https\?://[-%+./?A-Za-z0-9_]\+[A-Za-z0-9/]'
syn keyword VizCommentSpecial contained TODO FIXME CHECKME
  \ nextgroup=VizCommentSpecialPunct
syn match VizCommentSpecialPunct contained '[.:]'
hi link VizCommentSpecialPunct VizCommentSpecial
syn keyword VizCommentError contained XXX


" Highlighting Defaults
"
hi def link VizConditional Conditional
hi def link VizConnective Statement
hi def link VizDeclarator Statement
hi def link VizVerb Statement

hi def link VizPrecedence Special
hi def link VizOperator Operator
hi def link VizPathOperator Special

hi def link VizSigName Identifier
hi def link VizSctName Macro
hi def link VizTypeName Type
hi def link VizPronoun Identifier

hi def link VizLabel Label
hi def link VizInjName Constant
hi def link VizNumber Number
hi def link VizNumberBase Special
hi def link VizString String
hi def link VizStringEscape Special

hi def link VizComment Comment
hi def link VizCommentHeader Comment
hi def link VizCommentSectionNumber SpecialComment
hi def link VizCommentSectionSpacer VizComment
hi def link VizCommentSectionTitle VizCommentHeader
hi def link VizCommentSpecial SpecialComment
hi def link VizCommentURL SpecialComment
hi def link VizCommentError Error
