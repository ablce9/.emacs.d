# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# unlimited size
HISTSIZE=-1
HISTFILESIZE=-1

# set a fancy prompt (non-color, overwrite the one in /etc/profile)
PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '

. ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWCOLORHINTS=1
export PROMPT_COMMAND='__git_ps1 "\[\033[00;36m\]\u@\h\[\033[00m\]:\[\033[01;31m\]\W\\[\033[00m\]" " \\\$ "'

# export PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h \w\a\]$PS1"
PROMPT_COMMAND='__git_ps1 "\u@\h:\w" "\\\$ "'

. ~/.functions
. ~/.aliases

# bash completion
. /usr/share/bash-completion/bash_completion
. /usr/share/bash-completion/*

# remap keybaord, caps lock fucks me.
setxkbmap -option caps:ctrl_modifier
