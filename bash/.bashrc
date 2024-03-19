#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

# From https://wiki.archlinux.org/title/SSH_keys#SSH_agents
# Start the agent automatically and make sure that only
# one ssh-agent process runs at a time.
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! -f "$SSH_AUTH_SOCK" ]]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

# GHCup only provides a bash script (not for Fish):
source ~/.ghcup/env

# Our approach here is to use bash in passwd as our
# default sheel, then call out to fish.
exec fish
