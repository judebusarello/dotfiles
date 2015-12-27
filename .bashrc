set -o vi
alias ls='ls --color=auto'
alias la='ls -la'
alias vi='gvim'
alias vim='gvim'
alias twitch=twitch_fn

function twitch_fn {
	killall vlc
	livestreamer twitch.tv/$1 best
}
