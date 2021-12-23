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

" enable syntax highlighting
syntax enable

" set utf8 as standard encoding and en_us as the standard language
set encoding=utf8

" use unix as the standard file type
" set ffs=unix,dos,mac

:source ~/.config/nvim/plugins_install.vim
:source ~/.config/nvim/plugins_settings.vim
:source ~/.config/nvim/themes.vim
