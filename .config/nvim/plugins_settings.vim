"Auto-Reload Your Vimrc
" augroup reload_vimrc " {
"     autocmd!
"     autocmd BufWritePost $MYVIMRC source $MYVIMRC
" augroup END " }
"
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" vim-multiple-cursors
let g:multi_cursor_next_key="\<C-e>"

"vim-cpp
let g:cpp_class_scope_highlight = 1 "Highlighting of class scope is disabled by default. To enable set
let g:cpp_member_variable_highlight = 1 "Highlighting of member variables is disabled by default. To enable set
let g:cpp_experimental_simple_template_highlight = 1 "There are two ways to hightlight template functions. Either
"let g:cpp_experimental_template_highlight = 1 "which is a faster implementation but has some corner cases where it doesn't work.
let g:cpp_concepts_highlight = 1 "Highlighting of library concepts is enabled by
let c_no_curly_error=1 "Braces inside square brackets shown as error

" Tagbar keybinding to F8
nmap <f8> :TagbarOpenAutoClose<cr>

" NERDTree keybinding to CTRL + n
map <leader><tab> :NERDTreeToggle<CR>

" Quit after open file
let NERDTreeQuitOnOpen=1

" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1

" Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingwhitespace = 1

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"syntastic c++11/14/17
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = '-std=c++14 -Wall -Wextra'
let g:syntastic_cpp_checkers = ['clang']

let g:syntastic_enable_signs = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_always_populate_loc_list = 1

let g:syntastic_auto_jump = 1
let g:syntastic_enable_balloons = 1

let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_auto_refresh_includes = 1
" let g:syntastic_cpp_compiler = 'clang++'
" let g:syntastic_cpp_compiler_options = '-std=c++14 -stdlib=libc++'
" let g:syntastic_cpp_checkers = ['clang_check', 'gcc']

" " youcompleteme C- family
" let g:ycm_register_as_syntastic_checker = 1

" "YCM will put icons in Vim's gutter on lines that have a diagnostic set.
" "Turning this off will also turn off the YcmErrorLine and YcmWarningLine
" "highlighting
" let g:ycm_enable_diagnostic_signs = 1
" let g:ycm_enable_diagnostic_highlighting = 0
" let g:ycm_always_populate_location_list = 1 "default 0
" let g:ycm_open_loclist_on_ycm_diags = 1 "default 1


" let g:ycm_complete_in_strings = 1 "default 1
" let g:ycm_collect_identifiers_from_tags_files = 1 "default 0
" let g:ycm_path_to_python_interpreter = '' "default ''


" let g:ycm_server_use_vim_stdout = 0 "default 0 (logging to console)
" let g:ycm_server_log_level = 'info' "default info

" let g:ycm_global_ycm_extra_conf = "~/.vim/ycm_extra_conf.py"
" let g:ycm_confirm_extra_conf = 1


" let g:ycm_goto_buffer_command = 'same-buffer' "[ 'same-buffer', 'horizontal-split', 'vertical-split', 'new-tab' ]
" let g:ycm_filetype_whitelist = { '*': 1 }
" let g:ycm_key_invoke_completion = '<C-Space>'


" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"


let g:tmux_navigator_no_mappings = 1

nnoremap <silent> {c-h} :TmuxNavigateLeft<cr>
nnoremap <silent> {c-k} :TmuxNavigateDown<cr>
nnoremap <silent> {c-j} :TmuxNavigateUp<cr>
nnoremap <silent> {c-l} :TmuxNavigateRight<cr>
nnoremap <silent> {c-p} :TmuxNavigatePrevious<cr>

"vim-gitgutter
" let g:gitgutter_override_sign_column_highlight = 0

" And then either update your colorscheme's SignColumn highlight group or set it in your vimrc:
" highlight SignColumn ctermbg=whatever    " terminal Vim


"EasyMotion
" map <Leader> <Plug>(easymotion-prefix)

"CtrlP
" let g:ctrlp_map = '<c-p>'
" let g:ctrlp_cmd = 'CtrlP'

" nmap <leader>g :SCCompileAF -std=c++14 -Wall -Wextra -pedantic<cr>
" nmap <leader>gr :SCCompileRunAF -std=c++14 -Wall -Wextra -pedantic<cr>

" nmap <leader>c :SCCompileAF -std=c++14 -Wall -Wextra -pedantic -Werror<cr>
" nmap <leader>cr :SCCompileRunAF -std=c++14 -Wall -Wextra -pedantic -Werror<cr>
