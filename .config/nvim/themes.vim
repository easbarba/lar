colorscheme dracula

" True Colors
if has("termguicolors")

	set termguicolors
	" Escapes to get True Color work inside Tmux
	let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
	let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

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
