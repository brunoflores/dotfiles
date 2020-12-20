" https://vim-jp.org/vimdoc-en/autocmd.html#autocmd-events

" Colors
set termguicolors
colorscheme desert
highlight SpellBad guifg=White guibg=Red cterm=bold ctermfg=7 ctermbg=1

" Right margin
set colorcolumn=80
highlight ColorColumn ctermbg=236 guibg=#2c2d27

" Clipboard
set clipboard^=unnamed,unnamedplus

" Clear highlighting on return
nnoremap <CR> :noh<CR><CR>

" Sane splits
set splitbelow
set splitright

" Sets how many lines of history VIM has to remember
set history=500

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread
au FocusGained,BufEnter * checktime

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","

" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

"Always show current position
set ruler

" Height of the command bar
set cmdheight=2

" A buffer becomes hidden when it is abandoned
set hid

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Always show the status line
set laststatus=2

" Format the status line
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c

" Add a bit extra margin to the left
set foldcolumn=1

" Display line numbers
:set number

" Set spellfile to location that is guaranteed to exist, can be symlinked to
" Dropbox or kept in Git.
set spellfile=$HOME/.vim-spell-en.utf-8.add

" Delete trailing white spaces on save.
fun! CleanExtraSpaces()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    silent! %s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfun
autocmd BufWritePre *.md,*.txt,*.js,*.py,*.wiki,*.sh,*.coffee,*.v :call CleanExtraSpaces()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Verible
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !exists("g:verible_format")
    let g:verible_format = "verible-verilog-format"
endif
fun! VeribleFormat()
    silent execute "!" . g:verible_format . " --inplace " . bufname("%") . " &> /dev/null"
endfun
autocmd BufWritePost *.v :call VeribleFormat()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Fast editing and reloading of vimrc configs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <leader>e :e! ~/.vimrc<cr>
autocmd! bufwritepost ~/.vimrc source ~/.vimrc


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Turn persistent undo on
"    means that you can undo even when you close a buffer/VIM
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
try
    set undodir=~/.vim_runtime/temp_dirs/undodir
    set undofile
catch
endtry


" Makes grammar checker check comments only except for markdown and vim help.
let g:grammarous#default_comments_only_filetypes = {
            \ '*' : 1, 'help' : 0, 'markdown' : 0,
            \ }

" vim-plug
" https://github.com/junegunn/vim-plug
" https://github.com/junegunn/vim-plug/wiki/tips#automatic-installation
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'rhysd/vim-grammarous'
Plug 'editorconfig/editorconfig-vim'

" Initialize plugin system
call plug#end()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Helper functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction
