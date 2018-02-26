set nocompatible                                                                "This must be first, because it changes other options as a side effect.

call plug#begin('~/.vim/plugged')
Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
Plug 'airblade/vim-gitgutter'
Plug 'edkolev/tmuxline.vim'
Plug 'kchmck/vim-coffee-script'
Plug 'kien/ctrlp.vim'
Plug 'majutsushi/tagbar'
Plug 'ntpeters/vim-better-whitespace'
Plug 'scrooloose/nerdtree'
Plug 'sjl/gundo.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'dkprice/vim-easygrep'
Plug 'lambdalisue/vim-fullscreen'
Plug 'leafgarland/typescript-vim'
""Plug 'kristijanhusak/vim-hybrid-material'
""Plug 'nightsense/carbonized'
""Plug 'mkarmona/colorsbox'
""Plug 'nightsense/seagrey'
""Plug 'w0ng/vim-hybrid'
""Plug 'tyrannicaltoucan/vim-quantum'
""Plug 'chriskempson/vim-tomorrow-theme'
""Plug 'chriskempson/base16-vim'
Plug 'dikiaap/minimalist'
Plug 'tmux-plugins/vim-tmux-focus-events'
call plug#end()

syntax on
filetype plugin indent on
let mapleader=""
nmap <Leader><Leader> <c-^>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set splitright
nnoremap <C-Q> :hide<CR>
nnoremap <C-F> :vsplit<CR>

set ignorecase
set smartcase

set noswapfile
set backupdir=~/.vim/backup//
set directory=~/.vim/swap//
set undodir=~/.vim/undo//

set autochdir
set number

set tabstop=2
set shiftwidth=2
set shiftround
set expandtab

set scrolloff=15
set nowrap
set clipboard=unnamed

nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR> "space clears the search
set hlsearch
set incsearch

set updatetime=50 "helps tagbar update quicker

set background=dark
"" The lines below will refresh buffers according to the file on disk when you
"" switch over. Autoread doesn't flipping work in the terminal so you need the
"" hack
set autoread
au FocusGained,BufEnter * :silent! !

nnoremap <Leader>o :NERDTreeFind<CR>
nnoremap <Leader>O :NERDTreeToggle<CR>

"" autocmd QuickFixCmdPost *grep* cwindow

let g:tagbar_type_typescript = {
  \ 'ctagsbin' : 'tstags',
  \ 'ctagsargs' : '-f-',
  \ 'kinds': [
    \ 'e:enums:0:1',
    \ 'f:function:0:1',
    \ 't:typealias:0:1',
    \ 'M:Module:0:1',
    \ 'I:import:0:1',
    \ 'i:interface:0:1',
    \ 'C:class:0:1',
    \ 'm:method:0:1',
    \ 'p:property:0:1',
    \ 'v:variable:0:1',
    \ 'c:const:0:1',
  \ ],
  \ 'sort' : 0
  \ }

let g:ctrlp_by_filename = 0
let g:ctrlp_switch_buffer = 'ETVH'

nnoremap <Leader>J <C-]>
nnoremap <Leader>K <C-t>

let NERDTreeMinimalUI=1
let NERDTreeQuitOnOpen=1
let NERDTreeDirArrows = 1
let g:NERDTreeWinSize=45
let g:tagbar_compact=1
let g:tagbar_hide_nonpublic=1
let g:tagbar_left = 1
let g:tagbar_width=45

" " Sync Nerdtree when switching buffers
" autocmd BufWinEnter * :NERDTreeFind
" autocmd BufWinEnter * wincmd p

" This trigger takes advantage of the fact that the quickfix window can be
" easily distinguished by its file-type, qf. The wincmd J command is
" equivalent to the Ctrl+W, Shift+J shortcut telling Vim to move a window to
" the very bottom (see :help :wincmd and :help ^WJ).
" autocmd FileType qf wincmd J

set hidden

" let g:airline#extensions#tabline#alt_sep = 1
" let g:airline#extensions#tabline#show_buffers = 1
" let g:airline#extensions#tabline#buffer_idx_mode = 1

let g:airline_theme = "hybrid"
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

let g:airline_section_y = '%{(&fenc == "" ? &enc : &fenc)}'                     "set encoding type info
let g:airline_section_z = '%{substitute(getcwd(), expand("$HOME"), "~", "g")}'  "Set relative path
let g:airline#extensions#whitespace#enabled = 0                                 "Disable whitespace extension
let g:airline#extensions#tabline#left_sep = ' '                                 "Left separator for tabline
let g:airline#extensions#tabline#left_alt_sep = '│'                             "Right separator for tabline

nmap <Leader>1 <Plug>AirlineSelectTab1
nmap <Leader>2 <Plug>AirlineSelectTab2
nmap <Leader>3 <Plug>AirlineSelectTab3
nmap <Leader>4 <Plug>AirlineSelectTab4
nmap <Leader>5 <Plug>AirlineSelectTab5
nmap <Leader>6 <Plug>AirlineSelectTab6
nmap <Leader>7 <Plug>AirlineSelectTab7
nmap <Leader>8 <Plug>AirlineSelectTab8
nmap <Leader>9 <Plug>AirlineSelectTab9
nmap <leader>h <Plug>AirlineSelectPrevTab
nmap <leader>l <Plug>AirlineSelectNextTab

map <leader>q :bp<bar>sp<bar>bn<bar>bd<CR>. 

" " HACKS for my shitty colorscheme
"" highlight search term=bold cterm=bold ctermfg=blue ctermbg=10

nnoremap <F5> :GundoToggle<CR>

set showmatch                                                                   "Highlight matching bracket
set nostartofline                                                               "Jump to first non-blank character
set lazyredraw                                                                  "Do not redraw on registers and macros


"" EXPERIMENTING
"" let g:enable_bold_font = 1                                                      "Enable bold font in colorscheme

" ================ GUI options ====================

set guioptions-=m                                                               "remove menu bar
set guioptions-=T                                                               "remove toolbar
set guioptions-=L                                                               "remove left scrollbar when vertical split
set guioptions-=r                                                               "remove left scrollbar when vertical split
set guioptions-=l                                                               "remove left scrollbar
""set guifont=InconsolataForPowerline\ Nerd\ Font\ Medium\ 8                     "font setup
""set linespace=1                                                                "Set lineheight in gvim

" ================ Persistent Undo ==================

" Keep undo history across sessions, by storing in file.
silent !mkdir ~/.vim/backups > /dev/null 2>&1
set undodir=~/.vim/backups
set undofile


" ================ General Config ====================

set t_Co=256                                                                    "Set 256 colors
set title                                                                       "change the terminal's title
set backspace=indent,eol,start                                                  "Allow backspace in insert mode
set history=500                                                                 "Store lots of :cmdline history
set showcmd                                                                     "Show incomplete cmds down the bottom
set noshowmode                                                                  "Hide showmode because of the powerline plugin
set gdefault                                                                    "Set global flag for search and replace
set gcr=a:blinkon500-blinkwait500-blinkoff500                                   "Set cursor blinking rate
set cursorline                                                                  "Highlight current line
set mouse=a                                                                     "Enable mouse
set timeoutlen=1000 ttimeoutlen=200                                             "Reduce Command timeout for faster escape and O
set laststatus=2                                                                "Show statusbar
set fileencoding=utf-8 encoding=utf-8                                           "Set utf-8 encoding on write
"" set wrap                                                                        "Enable word wrap
set linebreak                                                                   "Wrap lines at convenient points
set listchars=tab:\ \ ,trail:·                                                  "Set trails for tabs and spaces
set list                                                                        "Enable listchars
set completeopt-=preview                                                        "Disable preview for autocomplete
set conceallevel=2 concealcursor=i                                              "neosnippets conceal marker

syntax on                                                                       "turn on syntax highlighting
"" colorscheme base16-material-dark
"" colorscheme hybrid_material

""colorscheme hybrid_material
"" colorscheme carbonized-dark
"" colorscheme seagrey-dark

""colorscheme hybrid_reverse
""colorscheme hybrid_material
""colorscheme colorsbox-stnight

autocmd FileType typescript :set makeprg='bazel build'

let g:typescript_compiler_binary = ''
let g:typescript_compiler_options = ''
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

"" whatev
set colorcolumn=80
let g:loaded_netrw = 1
let g:loaded_netrwPlugin = 1

""colorscheme quantum
""let g:quantum_black=1
""let g:airline_theme='quantum'
colorscheme minimalist
let g:airline_theme='angr'
" colorscheme base16-google-dark
