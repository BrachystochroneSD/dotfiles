set number relativenumber
set smartindent
set ignorecase
set incsearch
set smarttab
set encoding=utf-8
set wrap linebreak
set autowrite
set expandtab tabstop=4 shiftwidth=4

call plug#begin('~/.vim/plugins')
 Plug 'terryma/vim-multiple-cursors'
 Plug 'dylanaraps/wal.vim'
 Plug 'vim-airline/vim-airline'
call plug#end()

colorscheme wal
hi Normal guibg=NONE ctermbg=NONE

" map <C-m> <Esc>:split<CR>
" map <C-M> <Esc>:vsplit<CR>

" move windows
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Bind M-space to esc
nnoremap <Space> <NOP>
imap <M-Space> <Esc>

" emacs shortcuts
nmap <C-x><C-s> :w<CR>
map <C-x><C-c> <Esc>:wq<CR>
imap <C-x><C-s> <Esc>:w<CR>i

" monter/descendre la ligne courante ou la sélection avec J et K
nnoremap K :m .-2<CR>==
nnoremap J :m .+1<CR>==
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<cr>gv=gv

let &t_SI.="\e[5 q" "SI = INSERT mode
let &t_SR.="\e[4 q" "SR = REPLACE mode
let &t_EI.="\e[1 q" "EI = NORMAL mode (ELSE)

" custom bindings
nmap <C-z> u
nmap <C-M-z> <C-r>

nmap ù ^
vmap ù ^
nmap µ $
vmap µ $

imap <C-BS> <C-w>

" autocmd
autocmd BufWritePre * %s/\s\+$//e
autocmd BufWritePre * :retab

" clipboard

vmap <C-y> :!xclip -f -sel clip<CR>


" gitdiff
nmap <C-x><C-j> ]c
nmap <C-x><C-k> [c

nmap <C-x><C-r> :diffg RE<CR>]c
nmap <C-x><C-l> :diffg LO<CR>]c

nmap <C-l> zz
