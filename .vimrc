""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              GENERAL SETTINGS                              "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" hello
" vim plug
call plug#begin('~/.vim/plugged')
Plug 'editorconfig/editorconfig-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'morhetz/gruvbox'
Plug 'tpope/vim-fugitive'
Plug 'freitass/todo.txt-vim'
Plug 'w0rp/ale'
call plug#end()

" general
set exrc
set hlsearch
set enc=utf-8
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc
set belloff=all
set colorcolumn=80,120
set noswapfile
set shell=/bin/bash
set tags+=tags;\\~
set undolevels=1000
set nowrap
set laststatus=2
set number
set relativenumber
set clipboard=unnamedplus

" paths
set path+=/home/vernon/Devenv/notes/**
set path+=/home/vernon/.scripts/**

" ignore some stuff
set wildignore+=*.so,*.o,*.zip,*.pdf,*.png,*.jpg,*.jpeg
set wildignore+=*/.git/*,*/node_modules/*,*/vendor/*

" trim trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" leader
let mapleader="\<Space>"

" colorscheme
set t_Co=256
colorscheme gruvbox
set bg=dark

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 ULTISNIPS                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                    ALE                                     "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let b:ale_linters = ['eslint', 'stylelint', 'phpcs']

" ignore minified files.
let g:ale_pattern_options = {
\ '\.min\.js$': {'ale_linters': [], 'ale_fixers': []},
\ '\.min\.css$': {'ale_linters': [], 'ale_fixers': []},
\}

" Show 5 lines of errors.
let g:ale_list_window_size = 5


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   TASKS                                    "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" holds the global tasks.
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  HELPERS                                   "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" update current buffer.
nnoremap <C-S> :update<CR>

" allows me to copy and past across multiple vim instances (Tmux).
vmap <leader>y :w! /tmp/vitmp<CR>
nmap <leader>p :r! cat /tmp/vitmp<CR>

" workflow.
nnoremap <leader>gs :Git<CR>
nnoremap <leader>gp :Gpush<CR>
nnoremap <leader>gP :Gpull<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>f :find<space>
nnoremap <leader>b :buffer<space>
nnoremap <leader>d :find %:p:h<CR>
nnoremap <leader>D :e.<CR>
nnoremap <leader>h :noh<CR>
nnoremap <leader>i :r %:p:h<tab>
nnoremap <leader>pt :tab term<CR>
nnoremap <leader>s :vimgrep //g **/*.js<S-left><S-left><right>
nnoremap <leader>r yiw:.,$s/<C-r>"//gc<left><left><left>
vnoremap <leader>r y:.,$s/<C-r>"//gc<left><left><left>
nnoremap <C-tab> :tabnext<CR>

" terminal bindings.
tnoremap <C-tab> <C-\><C-n> :tabnext<CR>
tnoremap <Esc> <C-\><C-n>

" quick exit insert mode.
inoremap jj <Esc>
inoremap hh <Esc>
inoremap kk <Esc>
