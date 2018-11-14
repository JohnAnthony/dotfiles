let b:plugNew = 0
if !filereadable($HOME . '/.vim/autoload/plug.vim')
	let b:plugNew = 1
end

if b:plugNew == 1
	:silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
end

call plug#begin($HOME . '/.vim/plugged')
Plug 'fatih/vim-go'
Plug 'nsf/gocode'
Plug 'junegunn/fzf.vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'scrooloose/nerdtree'
Plug 'qpkorr/vim-bufkill'
Plug 'leafgarland/typescript-vim'
Plug 'flazz/vim-colorschemes'
Plug 't9md/vim-choosewin'
Plug 'kien/ctrlp.vim'
call plug#end()

:set encoding=utf-8
:set tabstop=4
:set wildmode=longest,list
:set wildmenu
:set number
:set colorcolumn=80
:set nobackup
:set nowritebackup

if b:plugNew == 1
	PlugUpdate --sync
	execute ':q'
	echo 'Your package manager was missing. Fixed!'
end

:nmap w <Plug>(choosewin)
:let g:choosewin_overlay_enable = 1
:colorscheme molokai
