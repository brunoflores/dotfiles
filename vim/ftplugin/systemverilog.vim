fun! YosysShow()
    execute "!yosys -p \"read_verilog " . bufname("%") . "; proc; opt; show;\" > /dev/null"
endfun

fun! YosysLint()
    execute "!yosys -p \"read_verilog " . bufname("%") . "; proc; opt;\""
endfun

nnoremap <C-Y>s :call YosysShow()<CR>
nnoremap <C-Y>l :call YosysLint()<CR>
