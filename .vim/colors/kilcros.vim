" Made by LAMMJohnson
  
set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "kilcros"
hi Normal               ctermfg=7
hi Comment              ctermfg=243
hi Constant             ctermfg=220
hi Special              ctermfg=226
hi Identifier			ctermfg=165             cterm=none
hi Statement			ctermfg=14
hi PreProc              ctermfg=196
hi Type                 ctermfg=165
hi Function             ctermfg=82
hi Repeat               term=underline  ctermfg=202
hi Operator             ctermfg=Red
hi Ignore               ctermfg=black
hi Error                term=reverse ctermbg=Red ctermfg=White
hi Todo                 term=standout ctermbg=Yellow ctermfg=Black
hi LineNr       		ctermfg=250     ctermbg=235
"hi CursorLine
hi CursorColumn			cterm=NONE ctermbg=darkred ctermfg=white

" Common groups that link to default highlighting.
hi link String			Constant
hi link Character       Constant
hi link Number			Constant
hi link Boolean			Constant
hi link Float           Number
hi link Conditional     Repeat
hi link Label           Statement
hi link Keyword			Statement
hi link Exception       Statement
hi link Include			PreProc
hi link Define			PreProc
hi link Macro           PreProc
hi link PreCondit       PreProc
hi link StorageClass    Type
hi link Structure       Type
hi link Typedef			Type
hi link Tag             Special
hi link SpecialChar     Special
hi link Delimiter       Special
hi link SpecialComment	Special
hi link Debug           Special

hi VertSplit 	ctermfg=250 ctermbg=black term=none cterm=none
hi StatusLineNC	ctermfg=250 ctermbg=black term=none cterm=none
hi StatusLine	ctermfg=blue ctermbg=black term=none cterm=none
hi ColorColumn	ctermbg=235 term=none cterm=none
