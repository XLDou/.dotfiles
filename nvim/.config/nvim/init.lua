vim.cmd("syntax enable")
vim.cmd("filetype plugin on")
vim.cmd("set path +=**")

vim.cmd("set wildmenu")

vim.cmd("syntax on")
vim.cmd("set number")
vim.cmd("set cursorline")
vim.cmd(":highlight Cursorline cterm=bold ctermbg=black")
vim.cmd("")
vim.cmd("set hlsearch")

vim.cmd("set tabstop     =4")
vim.cmd("set softtabstop =4")
vim.cmd("set shiftwidth  =4")
vim.cmd("set textwidth   =79")
vim.cmd("set expandtab")
vim.cmd("set autoindent")
vim.cmd("set showmatch")
vim.cmd("map <F6> :setlocal spell! spelllang=en_us<CR>")

require("config.lazy")
