set nocompatible                                                                "This should be the first line

" ================ Persistent Undo ==================

" Keep undo history across sessions, by storing in file.
silent !mkdir ~/.vim/backups > /dev/null 2>&1
set undodir=~/.vim/backups
set undofile

" ============= Basic Settings =============================================
" Terminal To Use In NVIM
set shell=/bin/zsh
let $SHELL = "/bin/zsh"
:tnoremap <Esc> <C-\><C-n>                                                      "Esc exits the terminal pane in neovim

" ================ General Config ====================
"
" Move between panes with ctrl-[hjkl]
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Default new splits to the right
set splitright

" Search without worrying about case unless specifically typing capital letters
" Need both ignore and smartcase
set ignorecase
set smartcase

"Prevent x from overwriting the clipboard selection
noremap x "_x
noremap X "_X

filetype plugin indent on                                                       "Guesses filetype for syntax highlighting and intentation on file load
nnoremap <CR> :noh<CR><CR>
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR> "space clears the search
set autochdir
set backspace=indent,eol,start                                                  "Allow backspace in insert mode
set clipboard=unnamedplus
set colorcolumn=80
set completeopt-=preview                                                        "Disable preview for autocomplete
set expandtab
set fileencoding=utf-8 encoding=utf-8                                           "Set utf-8 encoding on write
set gcr=a:blinkon500-blinkwait500-blinkoff500                                   "Set cursor blinking rate
set gdefault                                                                    "Set global flag for search and replace
set hidden
set history=500                                                                 "Store lots of :cmdline history
set hlsearch
set incsearch
set laststatus=2                                                                "Show statusbar
set lazyredraw                                                                  "Do not redraw on registers and macros. Helps with big macros on big files.
set linebreak                                                                   "Wrap lines at convenient points
set list                                                                        "Enable listchars
set listchars=tab:\ \ ,trail:·                                                  "Set trails for tabs and spaces
set mouse=a                                                                     "Enable mouse
set noruler
set noshowcmd                                                                   "Show incomplete cmds down the bottom
set noshowmode                                                                  "Hide showmode because of the powerline plugin
set nostartofline                                                               "Jump to first non-blank character when entering new line
set noswapfile                                                                  "Don't store swap files
set nowrap
set number
set shiftround
set shiftwidth=2
set showmatch                                                                   "Highlight matching bracket
set t_Co=256
set tabstop=2
set timeoutlen=1000 ttimeoutlen=200                                             "Reduce Command timeout for faster escape and O
syntax on                                                                       "Syntax highlighting (on)

"" The lines below will refresh buffers according to the file on disk when you
"" switch over. Autoread doesn't flipping work in the terminal so you need the
"" hack
set autoread
au FocusGained,BufEnter * :silent! !

" ============= Plugins ====================================================

" Download Vim Plug if it needs downloading
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" External Plugins
call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'                                                   "Marks diffed lines between last commit in the current file
Plug 'https://github.com/neoclide/coc.nvim', {'branch': 'release'}              "language server client/fzf tie-in for file searching
Plug 'https://github.com/ryanoasis/vim-devicons'                                "language logos per-filetype in fzf search windows
Plug 'joshdick/onedark.vim'                                                     "Colorscheme
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }                             "file navigation/general fuzzy finder
Plug 'junegunn/fzf.vim'
Plug 'kchmck/vim-coffee-script'                                                 "Coffeescript syntax highlighting
Plug 'leafgarland/typescript-vim'                                               "Typescript syntax highlighting
Plug 'rhysd/git-messenger.vim'                                                  "Floating Git-Blame Window in Nvim with ctrl-b
Plug 'tpope/vim-commentary'                                                     "Hit g,c to comment or uncomment a block of code
Plug 'tpope/vim-fugitive'                                                       "Needed to use FZF for branch searching/changing
Plug 'voldikss/vim-floaterm'                                                    "Floating Terminal
call plug#end()

" Extensions to the Language Server
let g:coc_global_extensions = [
  \  'coc-fzf-preview',
  \  'coc-go',
  \  'coc-java',
  \  'coc-python',
  \  'coc-tsserver',
  \  'coc-json',
  \  'coc-html',
  \  'coc-sh',
  \]

" ====================== Color Scheme =====================================

set t_Co=256                                                                    "Set 256 colors
colorscheme onedark                                                             "Colorscheme that looks like VSCode

" ====================== Git Gutter =======================================

" GitGutter styling to use · instead of +/-
let g:gitgutter_sign_added = '◆'
let g:gitgutter_sign_modified = '◆'
let g:gitgutter_sign_modified_removed = '◆'
let g:gitgutter_sign_removed = '◆'
let g:gitgutter_sign_removed_first_line = '◆'

" =================== Git Messenger =======================================
"
" Git Blame a current line Using CTRL-b
nnoremap <C-b> :GitMessenger<CR>

" =================== CoC Language Server =================================
"
" Next and Prev error with ctrl-g
nnoremap <C-g> :call CocAction('diagnosticNext')<CR>

" Jump to the definition with ctrl-t
nnoremap <C-t> :call CocAction('jumpDefinition', 'vsplit')<CR>


" Fuzzy Find all the places a given symbol is referenced
nnoremap <silent> <C-s> :CocCommand fzf-preview.CocReferences<CR>

" Show when coc is initiallizing the whatever
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}%F

" ================= FZF Native =============================================
" Search the contents of all the files in the git repo
nmap ; :GitGrep<CR>
command! -bang -nargs=* GitGrep
  \ call fzf#vim#ag(<q-args>,
  \  fzf#vim#with_preview({
  \    'options': '--no-hscroll --delimiter : --nth 4..',
  \    'dir': systemlist('git rev-parse --show-toplevel')[0]},
  \    'right:50%'
  \  ),
  \  <bang>0)

" search the contents of uncommitted files and files in the previous commit
" let preview = printf('git diff --color=always -- {-2} | sed 1,4d', s:bin.preview)
" nnoremap <silent> <C-\> :GitGrepCurrentCommit<CR>
" command! -bang -nargs=* GitGrepCurrentCommit
"   \ call fzf#vim#gitfiles('?',
"   \  {
"   \    'source': 'git diff --name-only HEAD^',
"   \    'sink': 'e',
"   \    'options': ['--no-hscroll', '--delimiter', ':', '--preview', 'if [[ {1} =~ M ]]; then %s; else %s {-1}; fi; git diff --color=always HEAD^'],
"   \    'dir': systemlist('git rev-parse --show-toplevel')[0]
"   \  }),
"   \  <bang>0)

" nnoremap <silent> <C-\> :GitGrepCurrentCommit<CR>
" command! -bang -nargs=* GitGrepCurrentCommit
"   \ call fzf#vim#gitfiles('?',
"   \  fzf#vim#with_preview({
"   \    'source': 'git diff --name-only HEAD^',
"   \    'sink': 'e',
"   \    'options': '--no-hscroll --delimiter : --nth 4..',
"   \    'dir': systemlist('git rev-parse --show-toplevel')[0]},
"   \    'right:50%'
"   \  ),
"   \  <bang>0)

" search the git repo for a filename
nnoremap <silent> <C-p> :GF<CR>

" search modified files
nnoremap <silent> <C-\> :GF?<CR>

" ================= Floaterm  =============================================
"
" Terminals to toggle in vim. Use ctrl-7, ctrl-8, ctrl-9
let g:floaterm_width = 0.8
noremap <C-y> :FloatermToggle term1<CR>
tnoremap <C-y> <C-\><C-n>:FloatermToggle term1<CR>
inoremap <C-y> <Esc>:FloatermToggle term1<CR>
noremap <C-n> :FloatermToggle term2<CR>
tnoremap <C-n> <C-\><C-n>:FloatermToggle term2<CR>
inoremap <C-n> <Esc>:FloatermToggle term2<CR>
" noremap <C-8> :FloatermToggle term2<CR>
" noremap <C-9> :FloatermToggle term3<CR>
" tnoremap <C-8> <C-\><C-n>:FloatermToggle term2<CR>
" tnoremap <C-9> <C-\><C-n>:FloatermToggle term3<CR>
" inoremap <C-8> <Esc>:FloatermToggle term2<CR>
" inoremap <C-9> <Esc>:FloatermToggle term3<CR>

" let g:floaterm_wintype = 'split'
" let g:floaterm_position = 'rightbelow'

" ================= Run Tests =============================================
" nnoremap <silent><nowait> <C-t> :term ~/docker-compose/dc-run inventory_api bazel test . <CR>
"
noremap <C-w> <Esc>:W<CR>

nnoremap <silent> <C-0> :tabm 0<CR>
nnoremap <silent> <C-1> :tabm 1<CR>
nnoremap <silent> <C-2> :tabm 2<CR>
