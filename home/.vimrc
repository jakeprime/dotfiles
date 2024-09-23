set nocompatible              " be iMproved, required
filetype off                  " required

if has("gui_running")
   let s:uname = system("uname")
   if s:uname == "Darwin\n"
      set guifont=Inconsolata\ for\ Powerline:h15
   endif
endif

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

set guifont=Inconsolata\ for\ Powerline:h15
let g:Powerline_symbols = 'fancy'
set encoding=utf-8
set t_Co=256
set fillchars+=stl:\ ,stlnc:\
set term=xterm-256color
set termencoding=utf-8

set ruler
inoremap jk <Esc>

call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Bundle 'powerline/powerline', {'rtp': 'powerline/bindings/vim/'}

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
"

" Enable 24-bit RGB colors for terminals
set termguicolors

" Define terminal escape sequences for insert and normal modes
if exists('$TERM')
    let &t_SI = "\e[6 q"  " Beam cursor (Insert mode)
    let &t_EI = "\e[2 q"  " Block cursor (Normal mode)
endif

" Reset the cursor to block on Vim startup
augroup SetCursorShapeOnStartup
  autocmd!
  autocmd VimEnter * silent! call SetCursorToBlock()
augroup END

" Function to force cursor to block
function! SetCursorToBlock()
  silent! echo -ne "\e[2 q"
endfunction
