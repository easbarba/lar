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
Plug 'altercation/vim-colors-solarized'
Plug 'tomasr/molokai'
Plug 'sjl/badwolf'
Plug 'morhetz/gruvbox'
Plug 'dracula/vim'

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
