" autocmd BufWrite *.hs :Autoformat
" " Don't automatically indent on save, since vim's autoindent for haskell is buggy
" autocmd FileType haskell let b:autoformat_autoindent=0

" setlocal formatprg=hindent
