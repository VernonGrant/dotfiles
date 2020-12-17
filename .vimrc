" ==========================================================================="
" GENERAL SETTINGS
" ==========================================================================="

" vim plug
call plug#begin('~/.vim/plugged')
Plug 'editorconfig/editorconfig-vim'
call plug#end()

" general
set backspace=indent,eol,start
set belloff=all
set clipboard=unnamed
set colorcolumn=80,120
set exrc
set hlsearch
set noswapfile
set ruler
set shell=/bin/zsh\ -l
set tags+=tags;\\~
set undolevels=1000

" file encoding
set enc=utf-8
set encoding=utf8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf8,prc

" paths
set path+=~/notes/**,~/vim-sessions/**

" is work conditions
set iskeyword+=-

" ignore some stuff.
set wildignore+=*.so,*.o,*.zip,*.pdf,*/.git/*,*/node_modules/*,*/vendor/*

" gui
set guioptions-=L,R,l,r
autocmd! GUIEnter * set vb t_vb=

" trim trailing spaces.
autocmd BufWritePre * :%s/\s\+$//e

" leader
let mapleader="\<Space>"

" theming
let macvim_skip_colorscheme = 1
set guifont=Menlo\ Regular:h15
set background=dark
colorscheme desert
highlight Normal guibg=#212121
highlight Search guifg=#ffffff guibg=#996228
highlight Comment guifg=#996228

" ==========================================================================="
" LINITING "
" ==========================================================================="

augroup Linting
	autocmd!
	" css,scss,sass
	autocmd FileType css setlocal makeprg=stylelint\ --formatter=unix\ %
	autocmd BufWritePost *.css,*.scss,*.sass silent make! <afile> | silent redraw!
	" javascript
	autocmd FileType javascript setlocal makeprg=eslint\ --format=unix\ %
	autocmd BufWritePost *.js silent make! <afile> | silent redraw!
	autocmd QuickFixCmdPost [^l]* cwindow
augroup END

" ==========================================================================="
" PROJECT SPECIFIC VIMRC "
" ==========================================================================="

" work on a more simple solution.
if !exists("*LoadProjectVimrc")
  function! LoadProjectVimrc()
    let vimrcFile = findfile(".vimrc", ".;")

    if !empty(vimrcFile)
      execute ":so" l:vimrcFile
      echom "A project specifc vimrc has been loaded."
    endif

  endfunction
endif
autocmd DirChanged * :call LoadProjectVimrc()

" ==========================================================================="
" HELPERS "
" ==========================================================================="

" write.
nnoremap <C-s> :write<CR>

" workflow.
nnoremap <leader>f :find<space>
nnoremap <leader>d :find %:p:h<CR>
nnoremap <leader>D :e.<CR>
nnoremap <leader>h :noh<CR>
nnoremap <leader>i :r %:p:h<tab>
nnoremap <leader>r yiw:.,$s/<C-r>"//gc<left><left><left>
vnoremap <leader>r y:.,$s/<C-r>"//gc<left><left><left>
nnoremap <C-tab> :tabnext<CR>
tnoremap <C-tab> <C-\><C-n> :tabnext<CR>
tnoremap <Esc> <C-\><C-n>

" quick exit insert mode.
inoremap jj <Esc>
inoremap hh <Esc>
inoremap kk <Esc>

" faster arrow navigation, up and down.
nnoremap <C-k> :-5<CR>
inoremap <C-k> <Esc>:-5<CR> i
nnoremap <C-j> :+5<CR>
inoremap <C-j> <Esc>:+5<CR> i
nnoremap <C-Up> :-5<CR>
inoremap <C-Up> <Esc>:-5<CR> i
nnoremap <C-Down> :+5<CR>
inoremap <C-Down> <Esc>:+5<CR> i
