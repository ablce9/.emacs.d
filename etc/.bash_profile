# .profile

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

. ~/.bashrc
