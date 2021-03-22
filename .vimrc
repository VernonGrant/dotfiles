""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                              GENERAL SETTINGS                              "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" vim plug
call plug#begin('~/.vim/plugged')
Plug 'editorconfig/editorconfig-vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
call plug#end()

" general
" TODO: Learn more about this setting, and check if you can replace the custom
" vimrc loading function you implemented.
" set exrc
set hlsearch
set enc=utf-8
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc
set belloff=all
set colorcolumn=80,120
set noswapfile
set shell=/bin/zsh\ -l
set tags+=tags;\\~
set undolevels=1000
set nowrap
set laststatus=2
set number
set relativenumber

" paths
set path+=~/notes/**,~/vim-sessions/**

" is work conditions
set iskeyword+=-

" ignore some stuff
set wildignore+=*.so,*.o,*.zip,*.pdf,*.png,*.jpg,*.jpeg
set wildignore+=*/.git/*,*/node_modules/*,*/vendor/*

" gui
set guioptions-=L,R,l,r
autocmd! GUIEnter * set vb t_vb=

" trim trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" leader
let mapleader="\<Space>"

" color scheme settings.
let macvim_skip_colorscheme = 1
set guifont=Menlo\ Regular:h16
set background=dark
colorscheme desert
highlight Normal guibg=#212121
highlight Search guifg=#ffffff guibg=#996228
highlight Todo guifg=#ffffff guibg=#4f7a28
highlight ColorColumn guibg=#181818
highlight Comment guifg=#996228
highlight Pmenu guibg=#181818
highlight PmenuSel guifg=indianred guibg=#181818
highlight VertSplit guibg=#181818 guifg=#996228


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                STATUS LINE                                 "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

highlight SLBackground guibg=#181818 guifg=#996228
highlight SLFileType guibg=indianred guifg=#663333
highlight SLBufNumber guibg=SeaGreen guifg=#003333
highlight SLLineNumber guibg=#80a0ff guifg=#003366

set statusline=\%#SLBackground#
set statusline+=\ %F
set statusline+=\%= " separator
set statusline+=\ %#SLFileType#
set statusline+=\ FT:\ %Y
set statusline+=\ %#SLBufNumber#
set statusline+=\ BN:\ %n
set statusline+=\ %#SLLineNumber#
set statusline+=\ LN:\ %l


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                 ULTISNIPS                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  LINTING                                   "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

fun RunQuickfixLinter(command)
	execute ':cex system("' . expand(a:command) . ' ' . expand("%") '") | copen'
endf

fun LintChangedFiles()
	" Lints all git changed files and add's them to the quickfix window.
endf

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                   TASKS                                    "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" holds the global tasks.
let g:GlobalTasks = {'Git Status:': '!git status'}
let g:GlobalTasks = extend(g:GlobalTasks, {'Docker Up': "tab term docker-\compose up"})
let g:GlobalTasks = extend(g:GlobalTasks, {'Lint - PHP': "call RunQuickfixLinter('phpcs --standard=PSR2 --report=emacs')"})
let g:GlobalTasks = extend(g:GlobalTasks, {'Lint - PHP (WordPress)': "call RunQuickfixLinter('phpcs --standard=WordPress --report=emacs')"})
let g:GlobalTasks = extend(g:GlobalTasks, {'Lint - JavaScript': "call RunQuickfixLinter('eslint --format=unix')"})
let g:GlobalTasks = extend(g:GlobalTasks, {'Lint - CSS/SASS': "call RunQuickfixLinter('stylelint --formatter=unix')"})

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
"                           PROJECT SPECIFIC VIMRC                           "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if !exists("*LoadProjectVimrc")
	function! LoadProjectVimrc()
		let vimrcFile = findfile(".vimrc", ".;")

		if !empty(l:vimrcFile)
			execute ":so" l:vimrcFile
			echom "A project specifc vimrc has been loaded."
		endif
	endfunction
endif
autocmd DirChanged * :call LoadProjectVimrc()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                                  HELPERS                                   "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" write
nnoremap <C-s> :write<CR>

" workflow
nnoremap <leader>f :find<space>
nnoremap <leader>b :find<space>
nnoremap <leader>d :find %:p:h<CR>
nnoremap <leader>D :e.<CR>
nnoremap <leader>h :noh<CR>
nnoremap <leader>i :r %:p:h<tab>
nnoremap <leader>pt :tab term<CR>
nnoremap <leader>s :vimgrep //g **/*.js<S-left><S-left><right>
nnoremap <leader>r yiw:.,$s/<C-r>"//gc<left><left><left>
vnoremap <leader>r y:.,$s/<C-r>"//gc<left><left><left>
nnoremap <C-tab> :tabnext<CR>
nnoremap ciq ci"
nnoremap ciQ ci'
nnoremap <leader>of :!open %:h<CR>

" terminal bindings
tnoremap <C-tab> <C-\><C-n> :tabnext<CR>
tnoremap <Esc> <C-\><C-n>

" quick exit insert mode
inoremap jj <Esc>
inoremap hh <Esc>
inoremap kk <Esc>
