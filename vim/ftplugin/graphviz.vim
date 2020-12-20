fun! DotIt()
    silent execute "!" . "dot " . bufname("%") . " -O"
endfun
augroup dot_it
    autocmd!
    autocmd BufWritePost *.gv :call DotIt()
augroup END
