" {{{ General

" Sets how many lines of history VIM has to remember
set history=500

" Enable filetype plugins
filetype on
filetype plugin on
filetype indent on
set modeline

" Set to auto read when a file is changed from the outside
set autoread

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","

" Write and quit with c-s and c-q
nnoremap <leader>s :w <cr>
nnoremap <leader>q :q <cr>

" Pressing Ctrl-L leaves insert mode in evim, so why not in regular vim, too.
inoremap <C-c> <Esc>

" Cut/Copy/Paste to work using the standard hotkeys
vmap <leader>u "+yy
vmap <leader>y "*yy
" vmap <C-v> c<ESC>"+p
" imap <C-v> <C-r><C-o>+
"
" Yanked words to clipboard
vnoremap <leader>k "*y
vnoremap <leader>j "+y

"Run Bash Sript inside vim
nnoremap <leader>ba :! bash %<cr>

" :W sudo saves the files " (useful for handling the permission-denied error)
" command W w !sudo tee % > /dev/null

" A simple mapping to break the line at the cursor by pressing Ctrl+Enter:
" nmap <c-cr> i[Ctrl+V][Enter][Ctrl+V][Esc][Enter]

" }}}

" {{{ VIM user interface

" set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Turn on the WiLd menu
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=.git\*,.hg\*,.svn\*
else
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

"Always show current position
set ruler

" Height of the command bar
set cmdheight=2

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" " Properly disable sound on errors on MacVim
" if has("gui_macvim")
"     autocmd GUIEnter * set vb t_vb=
" endif

" *****************************

" Add a bit extra margin to the left
set foldcolumn=1
setl foldmethod=marker
" Tweak the color of the fold display column
" au ColorScheme * hi FoldColumn cterm=bold ctermbg=233 ctermfg=146

set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points

" call alias inside vim
" set shell=/bin/bash\ -l

" }}}

" {{{ Files, backups and undo

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

" }}}

" {{{ Text, tab and indent related

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap line

"*****************************

"Fix indentation
nnoremap <silent> <F6> mzgg=G`z

" if has('gui_running')
"   set list listchars=tab:▶‒,nbsp:∙,trail:∙,extends:▶,precedes:◀
"     let &showbreak = '↳'
" else
"       set list listchars=tab:>-,nbsp:.,trail:.,extends:>,precedes:<
"         let &showbreak = '^'
"     endif
"
"
"
" set showbreak=\\

" }}}

" {{{ Visual mode related

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR

" }}}

" {{{ Moving around, tabs, windows and buffers

" " Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
" map <space> /
" map <c-space> ?
"
" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>
"
" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Close the current buffer
map <leader>bd :Bclose<cr>:tabclose<cxr>gT

" Close all the buffers
map <leader>bu :bufdo bd<cr>

map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
map <leader>t<leader> :tabnext

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
try
    set switchbuf=useopen,usetab,newtab
    set stal=2
catch
endtry

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" }}}

" {{{ Editing mappings

" Remap VIM 0 to first non-blank character
" map 0 ^

" Delete trailing white space on save, useful for some filetypes ;)
fun! CleanExtraSpaces()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    silent! %s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfun

if has("autocmd")
    autocmd BufWritePre *.txt,*.js,*.py,*.wiki,*.sh,*.coffee :call CleanExtraSpaces()
endif

" mapping F5 to remove whitespace trailing
nnoremap <silent> <F5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>
" Manually Remove whitespace traling  by typing command: / $

" Fix indentation
map <F7> mzgg=G`z

" }}}

" {{{ Spell checking

" Set spell check for en and br
set spelllang=en

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=


" }}}

" {{{ Misc

" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Quickly open a buffer for scribble
map <leader>o :e ~/buffer<cr>

" Quickly open a markdown buffer for scribble
map <leader>x :e ~/buffer.md<cr>

" Toggle paste mode on and off
" map <leader>pp :setlocal paste!<cr>

" Toggle auto-indenTING FOR CODE PAste
set pastetoggle=<F2>

"Fast editing and reloading of vimrc configs
nnoremap <leader>e :tabedit $MYVIMRC <cr>

" GUI related Fonts GVIM

" Set font according to system
if has("mac") || has("macunix")
    set gfn=Hack:h14,Source\ Code\ Pro:h15,Menlo:h15
elseif has("win16") || has("win32")
    set gfn=Hack:h14,Source\ Code\ Pro:h12,Bitstream\ Vera\ Sans\ Mono:h11
elseif has("gui_gtk2")
    set gfn=Hack\ 14,Source\ Code\ Pro\ 12,Bitstream\ Vera\ Sans\ Mono\ 11
elseif has("linux")
    " set gfn=Hack\ 14,Source\ Code\ Pro\ 12,Bitstream\ Vera\ Sans\ Mono\ 11
elseif has("unix")
    set gfn=Monospace\ 11
endif

" Disable scrollbars (real hackers don't use scrollbars for navigation!)
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar

" }}}

" {{{ Languages

" python
let g:pymode_python = 'python3'

" }}}

"{{{ Extensions

let plug_install = 0
let autoload_plug_path = stdpath('config') . '/autoload/plug.vim'
if !filereadable(autoload_plug_path)
        silent exe '!curl -fL --create-dirs -o ' . autoload_plug_path .
                \ ' https://raw.github.com/junegunn/vim-plug/master/plug.vim'
        execute 'source ' . fnameescape(autoload_plug_path)
        let plug_install = 1
endif
unlet autoload_plug_path

call plug#begin('~/.config/nvim/plugins')

" PLUGINS

" File browsing
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
" Plug 'ctrlpvim/ctrlp.vim'
" Plugin 'chrisbra/Colorizer'

" Git for vim
" Plug 'tpope/vim-fugitive'

" Various syntax
Plug 'scrooloose/syntastic'
Plug 'PotatoesMaster/i3-vim-syntax'

" Completition
" Plug 'valloric/youcompleteme'
" Plug 'ervandew/supertab'

" Latex and markdown
" Plugin 'plasticboy/vim-markdown'
" Plugin 'lervag/vimtex'
" Plugin 'xuhdev/vim-latex-live-preview'

" Commentaries
Plug 'scrooloose/nerdcommenter'
" Plug 'tpope/vim-commentary'
" Plug 'tomtom/tcomment_vim'

" Utilities
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'majutsushi/tagbar'
Plug 'easymotion/vim-easymotion'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'myusuf3/numbers.vim'
" Plug 'zhou13/vim-easyescape'

" Align Text
Plug 'junegunn/vim-easy-align'
"
" Highlight
Plug 'octol/vim-cpp-enhanced-highlight'
" Plug 'vim-scripts/Conque-GDB'

" Useful useless
Plug 'mhinz/vim-startify' " The fancy start screen for Vim.

" Diff handlers
Plug 'mhinz/vim-signify'
" Plug 'airblade/vim-gitgutter'

" Latex and markdown
" Plugin 'plasticboy/vim-markdown'
" Plugin 'lervag/vimtex'
" Plugin 'xuhdev/vim-latex-live-preview'
Plug 'junegunn/goyo.vim'

" ColorScheme
" Plug 'sonph/onehalf', {'rtp': 'vim/'}
Plug 'altercation/vim-colors-solarized'
" Plug 'arcticicestudio/nord-vim'
" Plug 'NLKNguyen/papercolor-theme'
" Plug 'rakr/vim-one'
" Plug 'dikiaap/minimalist'
" Plug 'godlygeek/csapprox '
" Activate Nord Vim when editing Java files
" Plug 'arcticicestudio/nord-vim', { 'for': 'java' }
" Plug 'arcticicestudio/nord-vim', { 'on':  'NERDTreeToggle' }
" Plug 'tomasr/molokai'
" Plug 'sjl/badwolf'
" Plug 'morhetz/gruvbox'
" Plug 'dracula/vim'

"  StatusLine
Plug 'itchyny/lightline.vim'
" Plug 'bling/vim-airline'
" Plug 'vim-airline/vim-airline-themes'

"fonts
Plug 'ryanoasis/vim-devicons'

" session management
" Plug 'tpope/vim-obsession' " Autosave Session.vim
" Plug 'dhruvasagar/vim-prosession'
" Plug 'gikmx/ctrlp-obsession'

" Tmux - Vim
" Plug 'christoomey/vim-tmux-navigator'
" Plug 'benmills/vimux'

" Plug 'terryma/vim-multiple-cursors'
" Plug 'xuhdev/singlecompile'
" Plug 'Yggdroot/indentLine'
" Plug 'wincent/command-t'
" Plug 'godlygeek/tabular' " align text



call plug#end()

if plug_install
        PlugInstall --sync
endif
unlet plug_install

" }}}

" {{{ Themes

" enable syntax highlighting
syntax enable

" set utf8 as standard encoding and en_us as the standard language
set encoding=utf8

" use unix as the standard file type
" set ffs=unix,dos,mac

colorscheme desert

" True Colors
if has("termguicolors")

        set termguicolors
        " Escapes to get True Color work inside Tmux
        let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
        let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

" Termite True Colors
" if &term=~'termite'
"
"     if has("termguicolors")
"
"         set termguicolors
"         let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
"         let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
"
"     endif
"
"     elseif &term=~'screen'
"
"         if has("termguicolors")
"
"             set termguicolors
"             let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
"             let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
"
"         endif
"
" endif
"

set cursorline

" status line
set laststatus=2

" lightline themes : :h g:lightline.colorscheme
let g:lightline = {
            \ 'colorscheme': 'default',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ],
            \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
            \ },
            \ 'component_function': {
            \   'gitbranch': 'fugitive#head'
            \ },
            \ }

" "airlines themes
" let g:airline_theme='zenburn'
" let g:airline_powerline_fonts = 1

" 256 colors
" set t_ut=

" if &term=~'rxvt-unicode-256color'
    "
    " if &term=~'screen-256color'
    "
    "     " set t_co=256
    "
    " endif
    "
" endif

" }}}

" {{{ Plugins - Settings

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

" }}}
