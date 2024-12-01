set nocompatible
syntax enable
filetype plugin on
"file finder
set path +=**
"tab show file
set wildmenu

syntax on
set number

set cursorline
:highlight Cursorline cterm=bold ctermbg=black

set hlsearch

"set tab
set tabstop     =4
set softtabstop =4
set shiftwidth  =4
set textwidth   =79
set expandtab
set autoindent

set showmatch

autocmd BufWritePre *.py :%s/\s\+$//e
autocmd BufWritePre *.c :%s/\s\+$//e
autocmd BufWritePre *.cpp :%s/\s\+$//e


if !has('gui_running')
        set t_Co=256
endif

set termguicolors
" Vim colorscheme "
set background=dark
colorscheme desert

" Spell check set to F6"
map <F6> :setlocal spell! spelllang=en_us<CR>

