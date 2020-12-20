" Automatically wrap at 72 characters and spell check commit messages
setlocal textwidth=80
setlocal spell


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Grammarous
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fun! GrammarCheck()
"     execute ":GrammarousCheck"
" endfun
" augroup md_grammar
"     autocmd!
"     autocmd BufWritePre *.md :call GrammarCheck()
" augroup END
