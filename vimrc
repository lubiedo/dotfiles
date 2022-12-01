" Vundle plugins
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'ycm-core/YouCompleteMe'
Plugin 'dense-analysis/ale'
Plugin 'morhetz/gruvbox'
Plugin 'junegunn/fzf'
call vundle#end()

" theme
autocmd vimenter * ++nested colorscheme gruvbox
set background=dark
autocmd vimenter * hi! Normal ctermbg=NONE guibg=NONE

" ale
let g:airline#extensions#ale#enabled = 1
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_open_list = 1
let g:ale_list_window_size = 5

" fzf
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
nnoremap <S-F> :FZF<CR>

" YCM
set completeopt+=popup

" global/visual
filetype plugin indent on
syntax enable
set sw=2 sts=2 ts=8 et
set smarttab autoindent expandtab
set number
set numberwidth=1
set backspace=indent,eol,start
set modeline
set complete-=i
let g:airline_theme='minimalist'

"searching
set incsearch
set hlsearch
set showmatch
set ignorecase smartcase

":W! will do a sudo :w
cmap W! w !sudo tee %<CR>
":R will reload the file
cmap R! e<CR>
" \+P to toggle paste mode
function! TogglePaste()
	if (&paste == 0) | set paste | else | set nopaste | endif
endfunction
nmap <leader>p :call TogglePaste()<cr>

"mouse
set mouse=a

"skeletons
augroup skeletons
  au!
  autocmd BufNewFile *.* silent! execute '0r ~/.vim/templates/skeleton.'.expand("<afile>:e")
augroup END

" toggle automatic relative numbers if on insert mode
augroup autornu
  au!
  autocmd BufNewFile,BufRead,ModeChanged * if &nu && mode() == "v" | set rnu | else | set nornu | end
augroup END

