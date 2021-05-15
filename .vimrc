" =======================================================================
" Notes:
" - If the system clipboard register is not available in terminal vim you can
"   install gvim. This will make the system clipboard available in terminal vim.
" - Using registers is a simple as {register} action.
" - When opening the quickfix window you can suffix it with a number to show a
"   certian number of lines. For example (:copen 1000)
" - You can execute a command in each file in the quickfix list using (:cfdo)
" =======================================================================

" =======================================================================
" Vim Plug
" =======================================================================

call plug#begin('~/.vim/plugged')
Plug 'SirVer/ultisnips'
Plug 'dart-lang/dart-vim-plugin'
Plug 'editorconfig/editorconfig-vim'
Plug 'morhetz/gruvbox'
Plug 'tpope/vim-fugitive'
Plug 'honza/vim-snippets'
Plug 'w0rp/ale'
call plug#end()

" =======================================================================
" General Configuration
" =======================================================================

let mapleader="\<Space>"

set belloff=all
set clipboard=unnamedplus
set colorcolumn=80,120
set exrc
set hlsearch
set laststatus=2
set noswapfile
set nowrap
set number
set relativenumber
set shell=/bin/bash
set tags+=tags;\\~
set undolevels=1000

" File encodings.
set enc=utf-8
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc

" Show whitespaces using the following characters.
set list listchars=nbsp:¬,tab:»·,trail:·,extends:>

" A file that matches with one of these patterns is ignored when expanding wildcards.
set wildignore+=*.so,*.o,*.zip,*.pdf,*.png,*.jpg,*.jpeg
set wildignore+=*/.git/*,*/node_modules/*,*/vendor/*

" trim trailing spaces on save.
autocmd BufWritePre * :%s/\s\+$//e

" TODO: Maybe find something better.
" Colorscheme.
set t_Co=256
set bg=dark
colorscheme gruvbox

" =======================================================================
" Plugin Settings
" =======================================================================

" Ultisnips.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" Ale.
let g:ale_list_window_size = 5
let b:ale_linters = ['eslint', 'stylelint', 'phpcs', 'dart']
let g:ale_pattern_options = {
\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
\}

" Dart.
let g:dart_style_guide = 2
let g:dart_format_on_save = 0

" =======================================================================
" Custom Tasks Feature
" =======================================================================

" Holds the global tasks.
let g:GlobalTasks = {'NPM Install:': "!npm install"}

fun GetNumberedDictKeys(taskDict)
	" Extracts the task keys and returns them as a numbered list.
	let counter = 1
	let taskList = ['Run Task:']
	for key in sort(keys(a:taskDict))
		let taskList = add(l:taskList, l:counter . ': ' . key)
		let counter+=1
	endfor
	return l:taskList
endf

fun RunGlobalTask()
	" Runs the given task number.
	let selection = inputlist(GetNumberedDictKeys(g:GlobalTasks))
	if (l:selection != 0 && l:selection <= len(g:GlobalTasks))
		let taskKeys = sort(keys(g:GlobalTasks))
		execute(g:GlobalTasks[l:taskKeys[l:selection - 1]])
	endif
endf

nnoremap <leader>t :call RunGlobalTask()<CR>

" =======================================================================
" Bindings
" =======================================================================

" General.
nnoremap <leader>f :find<space>
nnoremap <leader>d :find %:p:h<CR>
nnoremap <leader>b :buffer<space>
nnoremap <leader>h :noh<CR>

" Substitute.
nnoremap <leader>r yiw:%s/<C-r>"/<C-r>"/gc<left><left><left>
vnoremap <leader>r y:%s/<C-r>"/<C-r>"/gc<left><left><left>

" Vimgrep.
nnoremap <expr> <leader>s ':vimgrep "'. escape(input('Find: '), '"') .'" ' . getcwd() . '/**/*.' . expand("%:e")

" Terminal bindings.
tnoremap <C-tab> <C-\><C-n> :tabnext<CR>
tnoremap <Esc> <C-\><C-n>

" Quick exit insert mode.
inoremap jj <Esc>
inoremap hh <Esc>
inoremap kk <Esc>

" Fugitive.
nnoremap <leader>gs :Git<CR>
nnoremap <leader>gp :Gpush<CR>
nnoremap <leader>gP :Gpull<CR>
