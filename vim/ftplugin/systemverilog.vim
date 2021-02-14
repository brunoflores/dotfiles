fun! YosysSpacer()
    silent execute "!echo \"\""
    silent execute "!echo \"\""
    silent execute "!echo \"--------------------\""
endfun

fun! YosysShow()
    call YosysSpacer()
    execute "!yosys -p \"read_verilog -sv " . bufname("%") . "; proc; opt; show;\""
endfun

fun! YosysLint()
    call YosysSpacer()
    execute "!yosys -p \"read_verilog -sv " . bufname("%") . "; proc; opt;\""
endfun

fun! YosysWarns()
    call YosysSpacer()
    execute "!yosys -p \"read_verilog -sv " . bufname("%") . "; proc; opt;\" | grep -i warn"
endfun

nnoremap <C-Y>s :call YosysShow()<CR>
nnoremap <C-Y>l :call YosysLint()<CR>
nnoremap <C-Y>w :call YosysWarns()<CR>
