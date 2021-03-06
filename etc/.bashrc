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


# unlimited size
HISTSIZE=-1
HISTFILESIZE=-1

. ~/.git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUPSTREAM="auto"
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_DESCRIBE_STYLE="contains"
export GIT_PS1_HIDE_IF_PWD_IGNORED=1

# Colour thingy; use 'em when feel like colour thingy...
# set variable identifying the chroot you work in (used in the prompt below)
# if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
#     debian_chroot=$(cat /etc/debian_chroot)
# fi
# dist_version=$(lsb_release -a 2>&1 | \
#		   tr '[:upper:]' '[:lower:]' \
#		   | grep -E 'codename' | cut -d ':' -f 2 | tr -d '[:space:]')
# lsb_dist=$(lsb_release -a 2>&1 | \
#	       tr '[:upper:]' '[:lower:]' | \
#	       grep -E 'id' | cut -d ':' -f 2 | tr -d '[:space:]')
#
# PROMPT_COLOR=
# case "$(uname -m)-$lsb_dist-$dist_version" in
#     "x86_64-centos-6")
#	PROMPT_COLOR="\033[01;31m"
#	;;
#     "x86_64-centos-7")
#	PROMPT_COLOR="\033[00;31m"
#	;;
#     "s390x-ubuntu-bionic")
#	PROMPT_COLOR="\033[00;33m"
#	;;
#     "x86_64-debian-jessie")
#	PROMPT_COLOR="\033[00;34m"
#	;;
#     "x86_64-debian-stretch")
#	PROMPT_COLOR="\033[00;35m"
#	;;
#     "x86_64-fedora-28")
#	PROMPT_COLOR="\033[00;36m"
#	;;
# esac

PROMPT_COMMAND='__git_ps1 "\u@\h:\w" "\\\$ "'

if [ -f ~/.bash_env ]; then
    . ~/.bash_env
fi

. ~/.functions
. ~/.aliases

if [ -d "/usr/share/bash-completion/" ]; then
    . /usr/share/bash-completion/*
fi

xkbmap=$(which setxkbmap)
if [ -f "$xkbmap" ]; then
    # remap keybaord, caps lock fucks me.
    setxkbmap -option caps:ctrl_modifier
fi

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
