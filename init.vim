" Fish doesn't play all that well with others
set shell=/bin/bash
" let mapleader = "\<Space>"
let mapleader=","
let maplocalleader = "\\"

:filetype on
:filetype plugin on

" Themes
set termguicolors
let base16colorspace=256

" Sane splits
set splitbelow
set splitright

syntax on

" Margin to the left
set foldcolumn=0

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set clipboard=unnamed

" Remove all trailing whitespace on save.
autocmd BufWritePre * %s/\s\+$//e

" Right margin.
:set colorcolumn=80
highlight ColorColumn ctermbg=236 guibg=#2c2d27

" Display line numbers.
:set number
" :set relativenumber

" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Clear highlighting on return
nnoremap <CR> :noh<CR><CR>

" Set spellfile to location that is guaranteed to exist, can be symlinked to
" Dropbox or kept in Git.
set spellfile=$HOME/.vim-spell-en.utf-8.add

call plug#begin('~/.vim/plugged')
Plug 'chriskempson/base16-vim'
" Plug 'fatih/vim-go'
Plug 'neoclide/coc.nvim', {'branch': 'release', 'do': 'yarn install --frozen-lockfile'}
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'rhysd/vim-grammarous'
Plug 'editorconfig/editorconfig-vim'
" Plug 'neowit/vim-force.com'
Plug 'itchyny/lightline.vim'
Plug 'machakann/vim-highlightedyank'
Plug 'andymass/vim-matchup'
Plug 'rust-lang/rust.vim'
Plug 'majutsushi/tagbar'
Plug 'pboettch/vim-cmake-syntax'
Plug 'wlangstroth/vim-racket'
Plug 'hashivim/vim-terraform'
Plug 'lervag/vimtex'
Plug 'google/vim-jsonnet'

" OCaml
Plug 'vim-syntastic/syntastic'
Plug 'let-def/ocp-indent-vim'

" BlueSpec
Plug 'mtikekar/vim-bsv'

" Dafny
Plug 'mlr-msft/vim-loves-dafny', {'for': 'dafny'}

call plug#end()

" https://github.com/lervag/vimtex
let g:tex_flavor = 'latex'

" Theme base16
colorscheme base16-atelier-dune

" Go
" let g:go_fmt_command = "goimports"
" let g:go_fmt_autosave = 1
" autocmd BufWritePre *.go GoFmt

" Makes grammar checker check comments only except for markdown and vim help.
let g:grammarous#default_comments_only_filetypes = {
			\ '*' : 1, 'help' : 0, 'markdown' : 0,
			\ }

" Terraform
" Allow vim-terraform to align settings automatically with Tabularize:
" let g:terraform_align=1
" Allow vim-terraform to automatically format *.tf and *.tfvars files with
" terraform fmt:
" let g:terraform_fmt_on_save=1

" ----------------------------------------------------------------------------
" coc.nvim default settings
" ----------------------------------------------------------------------------
" if hidden is not set, TextEdit might fail.
set hidden
" Better display for messages
set cmdheight=2
" Smaller updatetime for CursorHold & CursorHoldI
set updatetime=300
" don't give |ins-completion-menu| messages.
set shortmess+=c
" always show signcolumns
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
			\ pumvisible() ? "\<C-n>" :
			\ <SID>check_back_space() ? "\<TAB>" :
			\ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use U to show documentation in preview window
nnoremap <silent> U :call <SID>show_documentation()<CR>

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" disable vim-go :GoDef short cut (gd)
" this is handled by LanguageClient [LC]
let g:go_def_mapping_enabled = 0

" Salesforce APEX
let g:apex_tooling_force_dot_com_path = '/Users/brunoflores/Downloads/tooling-force.com-0.4.4.0.jar'
let g:apex_backup_folder="/tmp/apex/backup"
let g:apex_temp_folder="/tmp/apex/gvim-deployment"
let g:apex_properties_folder="/Users/brunoflores/devel/salesforce"

" Rust
let g:rustfmt_autosave = 1
let g:rustfmt_command = "rustfmt +nightly"

" majutsushi/tagbar
let g:rust_use_custom_ctags_defs = 1  " if using rust.vim
let g:tagbar_type_rust = {
			\ 'ctagsbin' : '/usr/local/bin/ctags',
			\ 'ctagstype' : 'rust',
			\ 'kinds' : [
			\ 'n:modules',
			\ 's:structures:1',
			\ 'i:interfaces',
			\ 'c:implementations',
			\ 'f:functions:1',
			\ 'g:enumerations:1',
			\ 't:type aliases:1:0',
			\ 'v:constants:1:0',
			\ 'M:macros:1',
			\ 'm:fields:1:0',
			\ 'e:enum variants:1:0',
			\ 'P:methods:1',
			\ ],
			\ 'sro': '::',
			\ 'kind2scope' : {
			\ 'n': 'module',
			\ 's': 'struct',
			\ 'i': 'interface',
			\ 'c': 'implementation',
			\ 'f': 'function',
			\ 'g': 'enum',
			\ 't': 'typedef',
			\ 'v': 'variable',
			\ 'M': 'macro',
			\ 'm': 'field',
			\ 'e': 'enumerator',
			\ 'P': 'method',
			\ },
			\ }

" Remove netrw banner
let g:netrw_banner = 0

" Set tree the netrw default view
let g:netrw_liststyle = 3

" Set netrw to 25%
let g:netrw_winsize = 25

" Open fuzzy finder on <Space>
nnoremap <C-Space> :FZF<CR>
nmap <leader>; :Buffers<CR>

" Splits
nnoremap <C-S> :vsplit<CR>
nnoremap <C-X> :split<CR>

" Quit shortcut
nnoremap <C-Q> :q<CR>

" Undo
set undofile                " Save undos after file closes
set undodir=$HOME/.vim/undo " where to save undo histories

" Open new file adjacent to current file
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" <leader><leader> toggles between buffers
nnoremap <leader><leader> <c-^>

" Jump to last edit position on opening file
if has("autocmd")
	" https://stackoverflow.com/questions/31449496/vim-ignore-specifc-file-in-autocommand
	au BufReadPost * if expand('%:p') !~# '\m/\.git/' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Theme
" colorscheme desert
highlight SpellBad guifg=White guibg=Red cterm=bold ctermfg=7 ctermbg=1
" highlight LineNr guifg=Gray guibg=#2c2d27
" highlight Search guifg=Black guibg=Yellow
" highlight Visual guifg=Yellow guibg=Black
" highlight Pmenu guifg=grey guibg=black
" " From the plugin 'machakann/vim-highlightedyank'
" highlight HighlightedyankRegion guifg=Black guibg=Yellow

" OCaml
let g:opamshare = substitute(system('opam var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
let g:syntastic_ocaml_checkers = ['merlin']
" https://www.systutorials.com/auto-indenting-for-ocaml-code-in-vim-with-ocp-indent
au BufEnter *.ml setf ocaml
au BufEnter *.mli setf ocaml
au FileType ocaml call FT_ocaml()
function FT_ocaml()
	set textwidth=80
	set colorcolumn=80
	set shiftwidth=2
	set tabstop=2
	" ocp-indent with ocp-indent-vim
	let opamshare=system("opam config var share | tr -d '\n'")
	execute "autocmd FileType ocaml source".opamshare."/ocp-indent/vim/indent/ocaml.vim"

	filetype indent on
	filetype plugin indent on
endfunction

" https://stackoverflow.com/questions/15992163/how-to-tell-vim-to-auto-indent-before-saving
"
" Restore cursor position, window position, and last search after running a
" command.
function! Preserve(command)
	" Save the last search.
	let search = @/

	" Save the current cursor position.
	let cursor_position = getpos('.')

	" Save the current window position.
	normal! H
	let window_position = getpos('.')
	call setpos('.', cursor_position)

	" Execute the command.
	execute a:command

	" Restore the last search.
	let @/ = search

	" Restore the previous window position.
	call setpos('.', window_position)
	normal! zt

	" Restore the previous cursor position.
	call setpos('.', cursor_position)
endfunction
" Re-indent the whole buffer.
function! Indent()
	call Preserve('normal gg=G')
endfunction
" Indent on save hook
autocmd BufWritePre <buffer> call Indent()
