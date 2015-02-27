alias la='ls -la'
alias twitch=twitch_fn

function twitch_fn {
	killall vlc
	livestreamer twitch.tv/$1 best
}
