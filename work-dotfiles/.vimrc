set nocompatible                                                                "This must be first, because it changes other options as a side effect.

"Preload Vim Plug if it doesn't exist
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-gitgutter'                                                   "Marks diffed lines between last commit in the current file
Plug 'dikiaap/minimalist'                                                       "Colorscheme
Plug 'easymotion/vim-easymotion'                                                "Jump directly to the character you are looking at
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }               "FZF, the GOAT fuzzy searcher
Plug 'junegunn/fzf.vim'                                                         "FZF, the GOAT fuzzy searcher
Plug 'kchmck/vim-coffee-script'
Plug 'leafgarland/typescript-vim'                                               "Typescript syntax highlighting
Plug 'ntpeters/vim-better-whitespace'                                           "Highlights trailing whitespace
Plug 'takac/vim-hardtime'                                                       "Prevents you from using HJKL all the time
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'tpope/vim-commentary'                                                     "Hit g,c to comment or uncomment a block of code
Plug 'tpope/vim-eunuch'                                                         "Use :Move :Delete and other command line file commands from within vim
Plug 'tpope/vim-fugitive'                                                       "Needed to use FZF for branch searching/changing
Plug 'wincent/terminus'                                                         "Switches the cursor depending on if you're in insert/normal/replace mode
Plug 'itspriddle/ZoomWin'                                                       "Lets ctrl-w o reverse itself
Plug 'dkprice/vim-easygrep'                                                     "Find and replace
Plug 'idanarye/vim-merginal'
Plug 'zxqfl/tabnine-vim'
Plug 'owickstrom/vim-colors-paramount'
Plug 'pbrisbin/vim-colors-off'
" Plug 'morhetz/gruvbox'
"Plug 'chriskempson/base16-vim'
Plug 'andreypopp/vim-colors-plain'
Plug 'sjl/badwolf'
Plug 'KabbAmine/yowish.vim'
Plug 'mhartington/oceanic-next'
Plug 'rhysd/committia.vim'
Plug 'rhysd/git-messenger.vim'
call plug#end()


" ======================== Experimental =====================================

let g:hardtime_default_on = 1                                                   "Prevents you from using HJKL all the time (on)

"Allows Easymotion
let g:EasyMotion_do_mapping = 0                                                 " Disable default mappings
let g:EasyMotion_smartcase = 1                                                  " Lets you search for lowercase and it will match uppercase letters
let g:EasyMotion_use_smartsign_us = 1                                           " Lets you search with '5' if you want a '%' kind of like smart case, treats the symbols as uppercase numbers
nmap s <Plug>(easymotion-overwin-f)
vmap s <Plug>(easymotion-overwin-f)
nmap f <Plug>(easymotion-bd-w)
vmap f <Plug>(easymotion-bd-w)

" ======================== Vim Basic Config =================================

set statusline+=%F
"colorscheme minimalist                                                          "Active colorscheme
"colorscheme paramount                                                           "Active colorscheme
set t_Co=256
" if (has("termguicolors"))
"   set termguicolors
" endif
set t_md=                                                                       "Disable Annoying Bolds in OceanicNext
colorscheme OceanicNext
"colorscheme off                                                           "Active colorscheme
"colorscheme goodwolf
"colorscheme yowish
"colorscheme plain
"colorscheme base16-grayscale-dark                                                           "Active colorscheme
"colorscheme base16-google-dark
"colorscheme gruvbox
"colorscheme base16-default-dark
filetype plugin indent on                                                       "Guesses filetype for syntax highlighting and intentation on file load
" let mapleader=""                                                              "Maps the leader key to 'Enter'
nnoremap <CR> :noh<CR><CR>
set lazyredraw                                                                  "Do not redraw on registers and macros
set nostartofline                                                               "Jump to first non-blank character when entering new line
set noswapfile                                                                  "Don't store swap files
set showmatch                                                                   "Highlight matching bracket
syntax on                                                                       "Syntax highlighting (on)

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set splitright
set ignorecase
set smartcase

" set backupdir=~/.vim/backup//
" set directory=~/.vim/swap//
" set undodir=~/.vim/undo//

set autochdir
set number

set tabstop=2
set shiftwidth=2
set shiftround
set expandtab

" set scrolloff=15
set nowrap
set clipboard=unnamed

nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR> "space clears the search
set hlsearch
set incsearch

" set updatetime=50 "helps tagbar update quicker

set background=dark
"" The lines below will refresh buffers according to the file on disk when you
"" switch over. Autoread doesn't flipping work in the terminal so you need the
"" hack
set autoread
au FocusGained,BufEnter * :silent! !



" ================ File Navigation ====================

" -----------------------------------------------------
"
" FZF all the files in the git repo
nnoremap <C-p> :GitFiles<CR>
command! -bang -nargs=? -complete=dir GitFiles
  \ call fzf#vim#gitfiles(<q-args>, fzf#vim#with_preview('right:60%'), <bang>0)

" FZF all the definition tags in the git repo
nnoremap <C-t> :Tags<CR>
" Don't know how to get preview from this
" command! -bang -nargs=? -complete=dir Tags
"   \ call fzf#vim#tags(<q-args>, fzf#vim#with_preview('right:60%'), <bang>0)

" FZF all the CONTENTS of the files in the git repo
nmap ; :GitGrep<CR>
command! -bang -nargs=* GitGrep
  \ call fzf#vim#ag(<q-args>,
  \  fzf#vim#with_preview({
  \    'options': '--no-hscroll --delimiter : --nth 4..',
  \    'dir': systemlist('git rev-parse --show-toplevel')[0]},
  \    'right:60%'
  \  ),
  \  <bang>0)

" FZF the files in the current folder
" nnoremap <C-x> :BTags<CR>
nnoremap <C-x> :FZFNeigh<CR>
command! FZFNeigh call s:fzf_neighbouring_files()
function! s:fzf_neighbouring_files()
  let current_file =expand("%")
  let cwd = fnamemodify(current_file, ':p:h')
  let command = 'ag -g "" -f ' . cwd . ' --depth 0'

  call fzf#run({
        \ 'source': command,
        \ 'sink':   'e',
        \ 'options': '-m -x +s',
        \ 'window':  'enew' })
endfunction

" command! -bang -nargs=? -complete=dir FZFNeigh
"   \ call fzf#vim#files(<q-args>, fzf#vim#with_preview('right:60%'), <bang>0)

nnoremap <C-a>c :BCommits<CR>
nnoremap <C-b> :Buffers<CR>

" nnoremap <Leader>b :Gbranch<CR>
" command! -bang Gbranch call fzf#run({
"             \ 'source': 'git branch -a --no-color | grep -v "^\* " ',
"             \ 'sink': function('s:changebranch')
"             \ })
" function! s:changebranch(branch)
"     execute 'Git checkout' . a:branch
"     call feedkeys("i")
" endfunction


" ====================== Git Gutter =======================================

" GitGutter styling to use · instead of +/-
let g:gitgutter_sign_added = '◆'
let g:gitgutter_sign_modified = '◆'
let g:gitgutter_sign_modified_removed = '◆'
let g:gitgutter_sign_removed = '◆'
let g:gitgutter_sign_removed_first_line = '◆'

" =================== Sidebars =======================
"
" let NERDTreeMinimalUI=1
" let NERDTreeQuitOnOpen=1
" let NERDTreeDirArrows = 1
" let g:NERDTreeWinSize=45
" let g:tagbar_compact=1
" let g:tagbar_hide_nonpublic=1
" let g:tagbar_width=45
" noremap <S-U> :UndotreeToggle<CR>
" nnoremap <Leader>t :TagbarToggle<CR>
" nnoremap <Leader>o :NERDTreeToggle<CR>
" nnoremap <Leader>O :NERDTreeFind<CR>

" " Sync Nerdtree when switching buffers
" autocmd BufWinEnter * :NERDTreeFind
" autocmd BufWinEnter * wincmd p

" This trigger takes advantage of the fact that the quickfix window can be
" easily distinguished by its file-type, qf. The wincmd J command is
" equivalent to the Ctrl+W, Shift+J shortcut telling Vim to move a window to
" the very bottom (see :help :wincmd and :help ^WJ).
" autocmd FileType qf wincmd J

" set hidden

"let g:airline#extensions#tabline#alt_sep = 1
"let g:airline#extensions#tabline#show_buffers = 1
"let g:airline#extensions#tabline#buffer_idx_mode = 1

"let g:airline_theme = "hybrid"
"let g:airline_theme = "minimalist"
"let g:airline_powerline_fonts = 1
" let g:airline#extensions#tabline#enabled = 1

" let g:airline_section_y = '%{(&fenc == "" ? &enc : &fenc)}'                     "set encoding type info
" let g:airline_section_z = '%{substitute(getcwd(), expand("$HOME"), "~", "g")}'  "Set relative path
" let g:airline#extensions#whitespace#enabled = 0                                 "Disable whitespace extension
" let g:airline#extensions#tabline#left_sep = ' '                                 "Left separator for tabline
" let g:airline#extensions#tabline#left_alt_sep = '│'                             "Right separator for tabline

" nmap <Leader>1 <Plug>AirlineSelectTab1
" nmap <Leader>2 <Plug>AirlineSelectTab2
" nmap <Leader>3 <Plug>AirlineSelectTab3
" nmap <Leader>4 <Plug>AirlineSelectTab4
" nmap <Leader>5 <Plug>AirlineSelectTab5
" nmap <Leader>6 <Plug>AirlineSelectTab6
" nmap <Leader>7 <Plug>AirlineSelectTab7
" nmap <Leader>8 <Plug>AirlineSelectTab8
" nmap <Leader>9 <Plug>AirlineSelectTab9
" nmap <leader>h <Plug>AirlineSelectPrevTab
" nmap <leader>l <Plug>AirlineSelectNextTab

" map <leader>q :bp<bar>sp<bar>bn<bar>bd<CR>.

" " HACKS for my shitty colorscheme
"" highlight search term=bold cterm=bold ctermfg=blue ctermbg=10

""nnoremap <F5> :GundoToggle<CR>



"" EXPERIMENTING
"" let g:enable_bold_font = 1                                                      "Enable bold font in colorscheme

" ================ GUI options ====================

" set guioptions-=m                                                               "remove menu bar
" set guioptions-=T                                                               "remove toolbar
" set guioptions-=L                                                               "remove left scrollbar when vertical split
" set guioptions-=r                                                               "remove left scrollbar when vertical split
" set guioptions-=l                                                               "remove left scrollbar
""set guifont=InconsolataForPowerline\ Nerd\ Font\ Medium\ 8                     "font setup
""set linespace=1                                                                "Set lineheight in gvim

" ================ Persistent Undo ==================

" Keep undo history across sessions, by storing in file.
silent !mkdir ~/.vim/backups > /dev/null 2>&1
set undodir=~/.vim/backups
set undofile


" ================ General Config ====================

set t_Co=256                                                                    "Set 256 colors
set backspace=indent,eol,start                                                  "Allow backspace in insert mode
set history=500                                                                 "Store lots of :cmdline history
set noshowmode                                                                  "Hide showmode because of the powerline plugin
set noruler
set laststatus=0
set noshowcmd                                                                   "Show incomplete cmds down the bottom
set gdefault                                                                    "Set global flag for search and replace
set gcr=a:blinkon500-blinkwait500-blinkoff500                                   "Set cursor blinking rate
" set cursorline                                                                  "Highlight current line
set mouse=a                                                                     "Enable mouse
set timeoutlen=1000 ttimeoutlen=200                                             "Reduce Command timeout for faster escape and O
set laststatus=2                                                                "Show statusbar
set fileencoding=utf-8 encoding=utf-8                                           "Set utf-8 encoding on write
"" set wrap                                                                        "Enable word wrap
set linebreak                                                                   "Wrap lines at convenient points
set listchars=tab:\ \ ,trail:·                                                  "Set trails for tabs and spaces
set list                                                                        "Enable listchars
set completeopt-=preview                                                        "Disable preview for autocomplete
" set conceallevel=2 concealcursor=i                                              "neosnippets conceal marker

syntax on                                                                       "turn on syntax highlighting
"" colorscheme base16-material-dark
"" colorscheme hybrid_material

""colorscheme hybrid_material
"" colorscheme carbonized-dark
"" colorscheme seagrey-dark

""colorscheme hybrid_reverse
""colorscheme hybrid_material
""colorscheme colorsbox-stnight

"" autocmd FileType typescript :set makeprg='bazel build'

" let g:typescript_compiler_binary = ''
" let g:typescript_compiler_options = ''
" autocmd QuickFixCmdPost [^l]* nested cwindow
" autocmd QuickFixCmdPost    l* nested lwindow

"" whatev
set colorcolumn=80
" let g:loaded_netrw = 1
" let g:loaded_netrwPlugin = 1

""colorscheme quantum
""let g:quantum_black=1
""let g:airline_theme='quantum'
""colorscheme crayon
"" colorscheme nord
""colorscheme jellybeans
"let g:airline_theme='angr'


" let g:sidepanel_pos = "left"
" " Set width if neccesary (default: 32)
" let g:sidepanel_width = 45

" " To use rabbit-ui.vim
" let g:sidepanel_use_rabbit_ui = 1

" Activate plugins in SidePanel
" let g:sidepanel_config = {}
" let g:sidepanel_config['nerdtree'] = {}
" let g:sidepanel_config['tagbar'] = {}
" let g:sidepanel_config['gundo'] = {}
" let g:sidepanel_config['buffergator'] = {}
" let g:sidepanel_config['vimfiler'] = {}

" let g:gundo_prefer_python3 = 1                                                  "allow gundo to work with only python3 installed

" noremap <Tab> :WSNext<CR>
" noremap <S-Tab> :WSPrev<CR>
" noremap <Leader><Tab> :WSClose<CR>
" noremap <Leader><S-Tab> :WSClose!<CR>
" noremap <C-t> :WSTabNew<CR>

" cabbrev bonly WSBufOnly

" let g:workspace_powerline_separators = 1
" let g:workspace_tab_icon = "\uf00a"
" let g:workspace_left_trunc_icon = "\uf0a8"
" let g:workspace_right_trunc_icon = "\uf0a9"

nnoremap <Leader>VV <C-]>
nnoremap <Leader>T <C-t>

" autocmd FileType qf wincmd J                                                    "Fix for quickfix window appearing within tagbar

" nmap <Leader>l <Plug>(Limelight)
" xmap <Leader>l <Plug>(Limelight)

" let g:goyo_width = 120
" let g:goyo_height = 90
" let g:goyo_linenr = 1
" nnoremap <C-g> :Goyo<CR>
" autocmd! User GoyoEnter Limelight
" autocmd! User GoyoLeave Limelight!

" nmap     <Leader>g :Gstatus<CR>gg<c-n>
""nnoremap <Leader>d :Gdiff<CR>
""nnoremap <Leader>D :Gdiff HEAD^<CR>
" nnoremap <C-s> :Goyo<CR>
nnoremap <C-c> :BCommits<CR>

"nnoremap <Leader>b :BLines<CR>






""rg -g '*.{toml,md,yml}' -c ripgrep

" nnoremap <Leader>d :FZFDirty<CR>
" command! FZFDirty call s:fzf_dirty_files()
" function! s:fzf_dirty_files()
"   let command = 'git status -s | cut -c4-'
"   let options = fzf#vim#with_preview('up:80%', '?').options

"   call fzf#run({
"         \ 'source': command,
"         \ 'sink':   'e',
"         \ 'options': options,
"         \ 'window':  'enew' })

" endfunction

" let s:bin_dir = expand('<sfile>:h:h:h').'home/judebusarello/.vim/plugged/fzf.vim/bin/'
" let s:bin = {'preview': s:bin_dir.('preview.sh')}

nnoremap <Leader>d :FZFDirty<CR>
command! FZFDirty call s:fzf_dirty_files()
function! s:fzf_dirty_files()
  let command = 'git status -s | cut -c4-'
  ""let options = fzf#vim#with_preview('up:80%', '?').options
  ""let options = ['--preview-window', 'up:80%', '--preview', (fzf#shellescape(s:bin.preview)).' {} | grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git diff -W --color=always {}']
  let options = ['--preview-window', 'up:80%', '--preview', (fzf#shellescape(s:bin.preview)).' {} | grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git diff --color=always {}']

  call fzf#run({
        \ 'source': command,
        \ 'sink':   'e',
        \ 'options': options,
        \ 'window':  'enew' })

endfunction

nnoremap <Leader>f :FZFFilthy<CR>
command! FZFFilthy call s:fzf_filthy_files()
function! s:fzf_filthy_files()
  let command = 'git show --stat --oneline HEAD^ | cut -c4-'
  let options = ['--preview-window', 'up:80%', '--preview', (fzf#shellescape(s:bin.preview)).' {} | grep -o "[a-f0-9]\{7,\}" <<< {} | xargs git diff --color=always {}']

  call fzf#run({
        \ 'source': command,
        \ 'sink':   'e',
        \ 'options': options,
        \ 'window':  'enew' })

endfunction

" Another form of FZFDirty
" nnoremap <Leader>d :Fzfc<CR>
" command! Fzfc call fzf#run(fzf#wrap(
"   \ {'source': 'git ls-files --exclude-standard --others --modified'}))
" noremap <Leader>] :Fzfc<cr>

" " Reads all the files in the git repo. Used for grepping with FZF.
" nnoremap <C-o> :ModifiedFiles<CR>
" command! ModifiedFiles execute 'GitFiles' s:files_modified_in_last_commit()
" function! s:files_modified_in_last_commit()
"   return system('git diff --name-status HEAD HEAD~1')[:-2]
" endfunction

" command! FZFMru call fzf#run({
" \  'source':  v:oldfiles,
" \  'sink':    'e',
" \  'options': '-m -x +s',
" \  'down':    '40%'})
"
" let g:fzf_layout = { 'down': '~100%' }
