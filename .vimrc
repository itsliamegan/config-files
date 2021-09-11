"--------"
" BASICS "
"--------"

" Disable legacy vi compatibility and set character encoding.
set nocompatible
set encoding=utf-8

"---------"
" PLUGINS "
"---------"

call plug#begin('~/.vim/plug')
  " Text objects and motions.
  Plug 'adelarsq/vim-matchit'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-endwise'
  Plug 'tpope/vim-surround'
  Plug 'wellle/targets.vim'

  " Make search more ergonomic.
  Plug 'romainl/vim-cool'

  " Use tab key for all completions.
  Plug 'ervandew/supertab'

  " Fuzzy file finding.
  Plug 'junegunn/fzf'
  Plug 'junegunn/fzf.vim'

  " Testing.
  Plug 'vim-test/vim-test'

  " Language-specific.
  Plug 'elixir-editors/vim-elixir'
	Plug 'dag/vim-fish'

  " GUI
  Plug 'jonathanfilip/vim-lucius'
call plug#end()

"------------------"
" FILES AND EDITOR "
"------------------"

" File syntax highlighting, detection, and indenting.
syntax on
filetype plugin indent on

" Show both absolute line numbers and relative line numbers.
set number
set relativenumber

" Use tabs as indentation..
set tabstop=2
set shiftwidth=2
set noexpandtab

" Improve search to be incremental and case-insensitive unless searches contain
" capitals.
set hlsearch
set incsearch
set ignorecase
set smartcase

" Automatically wrap at 80 characters.
set textwidth=80
set formatoptions+=t

" Restore normal backspace.
set backspace=indent,eol,start

" Don't make swap files.
set noswapfile

" Make new splits below and to the right.
set splitbelow
set splitright

" Use %% to expand to the directory of the current file.
cabbr <expr> %% expand('%:p:h')

" Reduce delay when hitting escape.
set ttimeoutlen=25

" Run the given command and restore the cursor position afterwards.
function! KeepPosition(command)
  let l:pos = getpos('.')
  execute a:command
  call setpos('.', l:pos)
endfunction

" Automatically trim trailing whitespace on save.
autocmd BufWritePre * call KeepPosition("%s/\s\+$//e")

" Utility to get the highlight groups under the cursor.
function! CurrentHighlightGroups()
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunction

"-----------"
" LANGUAGES "
"-----------"

" Format on save.
autocmd BufWritePre *.ex,*.exs silent! call KeepPosition("mix format -")
autocmd BufWritePre *.go silent! call KeepPosition("go fmt data.go")

function! HighlightMath()
    " Inline math.
    syn match InlineMath "\$[^$].\{-}\$"
    " Block math.
    syn region BlockMath   start=/\$\$/              end=/\$\$/
    syn region AlignedMath start=/\\begin{align\*}/ end=/\\end{align\*}/

    hi link InlineMath   Function
    hi link BlockMath    Statement
    hi link AlignedMath  BlockMath
endfunction

autocmd BufRead,BufNewFile,BufEnter *.md call HighlightMath()

"---------------"
" FUZZY FINDING "
"---------------"

let g:fzf_command_prefix = "Fzf"
let g:fzf_layout = { "down": "~40%" }
let g:fzf_preview_window = []
let g:fzf_files_options = "--reverse"

"-----"
" GUI "
"-----"

" Disable the visual & audio bell.
set vb t_vb=

" Ensure that comments are rendered in normal font.
highlight Comment cterm=none gui=none

" Use a color scheme.
set background=dark
colorscheme lucius
hi link rubyStringDelimiter String
hi link htmlTag htmlTagName
hi link htmlEndTag htmlTagName

" Fancy status line.
set laststatus=2
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)

"-------------"
" KEYBINDINGS "
"-------------"

" Set the leader key to Space.
let mapleader = " "

" Switching buffers.
nnoremap gn :bn<CR>
nnoremap gp :bp<CR>

" Fuzzy finding.
map <C-p> :FzfFiles<CR>

" Testing.
nnoremap <leader>t :TestFile<CR>
nnoremap <leader>a :TestSuite<CR>
nnoremap <leader>l :TestLast<CR>
