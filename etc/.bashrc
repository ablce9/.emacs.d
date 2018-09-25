umask 022

shopt -s autocd                  2>/dev/null
shopt -s cdable_vars		 2>/dev/null
shopt -s cdspell		 2>/dev/null
shopt -s checkhash		 2>/dev/null
shopt -s checkjobs		 2>/dev/null
shopt -s checkwinsize		 2>/dev/null
shopt -s cmdhist		 2>/dev/null
shopt -s compat31		 2>/dev/null
shopt -s compat32		 2>/dev/null
shopt -s compat40		 2>/dev/null
shopt -s compat41		 2>/dev/null
shopt -s compat42		 2>/dev/null
shopt -s compat43		 2>/dev/null
shopt -s complete_fullquote      2>/dev/null
shopt -u direxpand		 2>/dev/null
shopt -u dirspell		 2>/dev/null
shopt -u dotglob		 2>/dev/null
shopt -u execfail		 2>/dev/null
shopt -s expand_aliases		 2>/dev/null
shopt -u extdebug		 2>/dev/null
shopt -s extglob		 2>/dev/null
shopt -s extquote		 2>/dev/null
shopt -u failglob		 2>/dev/null
shopt -s force_fignore		 2>/dev/null
shopt -u globasciiranges         2>/dev/null
shopt -s globstar		 2>/dev/null
shopt -u gnu_errfmt		 2>/dev/null
shopt -s histappend		 2>/dev/null
shopt -u histreedit		 2>/dev/null
shopt -u histverify		 2>/dev/null
shopt -u hostcomplete		 2>/dev/null
shopt -u huponexit		 2>/dev/null
shopt -u inherit_errexit         2>/dev/null
shopt -s interactive_comments    2>/dev/null
shopt -u lastpipe		 2>/dev/null
shopt -u lithist		 2>/dev/null
shopt -s login_shell		 2>/dev/null
shopt -u mailwarn		 2>/dev/null
shopt -u no_empty_cmd_completion 2>/dev/null
shopt -u nocaseglob		 2>/dev/null
shopt -u nocasematch		 2>/dev/null
shopt -u nullglob		 2>/dev/null
shopt -s progcomp		 2>/dev/null
shopt -s promptvars		 2>/dev/null
shopt -u restricted_shell        2>/dev/null
shopt -u shift_verbose		 2>/dev/null
shopt -s sourcepath		 2>/dev/null
shopt -u xpg_echo                2>/dev/null

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

if [ -f ~/.bash_env ]; then
    . ~/.bash_env
fi

. ~/.functions
. ~/.aliases

# bash completion
. /usr/share/bash-completion/bash_completion
. /usr/share/bash-completion/*

# remap keybaord, caps lock fucks me.
setxkbmap -option caps:ctrl_modifier
