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
call plug#end()

" Extensions to the Language Server
let g:coc_global_extensions = [
  \  'coc-fzf-preview',
  \  'coc-go',
  \  'coc-java',
  \  'coc-python',
  \  'coc-tsserver',
  \  'coc-sql',
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

" search the git repo for a filename
nnoremap <silent><nowait> <C-p> :CocCommand fzf-preview.FromResources git<CR>

" Fuzzy Find all the places a given symbol is referenced
nnoremap <silent><nowait> <C-s> :CocCommand fzf-preview.CocReferences<CR>

" Show when coc is initiallizing the whatever
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" ================= FZF Native =============================================
" FZF all the CONTENTS of the files in the git repo
nmap ; :GitGrep<CR>
command! -bang -nargs=* GitGrep
  \ call fzf#vim#ag(<q-args>,
  \  fzf#vim#with_preview({
  \    'options': '--no-hscroll --delimiter : --nth 4..',
  \    'dir': systemlist('git rev-parse --show-toplevel')[0]},
  \    'right:50%'
  \  ),
  \  <bang>0)

" ==================== FZF Preview ========================================
"
function! s:find_git_root()
  return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction

let g:fzf_preview_directory_files_command = 'rg --files --hidden --follow --no-messages ' . s:find_git_root() " Installed ripgrep
let g:fzf_preview_git_files_command = 'rg --files --hidden --follow --no-messages ' . s:find_git_root() " Installed ripgrep
let g:fzf_preview_grep_cmd = 'rg --line-number --no-heading --color=never'

" Use vim-devicons in the filesearch window
let g:fzf_preview_use_dev_icons = 1
let g:fzf_preview_dev_icon_prefix_length = 2
let g:fzf_preview_dev_icons_limit = 100000

" jump to the buffers by default when possible
let g:fzf_preview_buffers_jump = 1

" floating window size ratio
let g:fzf_preview_floating_window_rate = 0.7

