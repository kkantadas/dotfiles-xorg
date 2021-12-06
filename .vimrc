syntax on

set path+=**
set nocompatible
set clipboard=unnamedplus
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set smartcase
set undodir=~/.vim/undodir
set undofile
set incsearch
set scrolloff=8
set hidden
set noswapfile
set nobackup
set updatetime=50
set cmdheight=2
set shortmess+=c
set wildmenu
"set t_Co=256
set fillchars=vert:.
set t_Co=16
set linebreak
set display=lastline
"set spell spelllang=en_us,de_de
"set formatoptions+=a

imap <silent> <Down> <C-o>gj
imap <silent> <Up> <C-o>gk
nmap <silent> <Down> gj
nmap <silent> <Up> gk


call plug#begin('~/.vim/plugged') 
Plug 'ervandew/supertab'
"Plug 'valloric/youcompleteme'
Plug 'sedm0784/vim-you-autocorrect'
Plug 'tpope/vim-fugitive'
Plug 'jceb/vim-orgmode'
Plug 'morhetz/gruvbox'
call plug#end()


" Coloring
autocmd colorscheme * hi clear SpellBad
autocmd colorscheme * hi SpellBad cterm=underline


hi clear SpellBad " Setting up the highlighting style to only underline
hi SpellBad ctermfg=NONE ctermbg=NONE cterm=underline
set background=dark

"colorscheme gruvbox  
"colorscheme elflord
colorscheme default

set foldcolumn=1
hi FoldColumn ctermbg=none

highlight VertSplit cterm=NONE

highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE
set syntax=scheme
nnoremap <F2>c :colorscheme solarized :hi Normal ctermbg=NONE<F9>
nnoremap <F12>c :exe ':silent !chromium %'<CR>
if v:version >= 600
  filetype plugin on
  filetype indent on
else
  filetype on
endif


"let g:slime_target = "stumpwm"
set mouse=c
augroup NO_CURSOR_MOVE_ON_FOCUS
  au!
  au FocusLost * let g:oldmouse=&mouse | set mouse=
  au FocusGained * if exists('g:oldmouse') | let &mouse=g:oldmouse | unlet g:oldmouse | endif
augroup END

" Vim respect XDG ...https://tlvince.com/vim-respect-xdg

"set directory=$XDG_CACHE_HOME/vim,~/,/tmp
"set backupdir=$XDG_CACHE_HOME/vim,~/,/tmp
"set viminfo+=n$XDG_CACHE_HOME/vim/viminfo
"set runtimepath=$XDG_CONFIG_HOME/vim,$XDG_CONFIG_HOME/vim/after,$VIM,$VIMRUNTIME
"let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc"


" jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif

" Do this in normal mode...
"nnouremap <Left>  :echoe "Use h"<CR>
"nnoremap <Right> :echoe "Use l"<CR>
"nnoremap <Up>    :echoe "Use k"<CR>
"   nnoremap <Down>  :echoe "Use j"<CR>
" ...and in insert mode
"inoremap <Left>  <ESC>:echoe "Use h"<CR>
"inoremap <Right> <ESC>:echoe "Use l"<CR>
"inoremap <Up>    <ESC>:echoe "Use k"<CR>
"inoremap <Down>  <ESC>:echoe "Use j"<CR>
