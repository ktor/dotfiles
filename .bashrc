# vi mode
set -o vi

# history
HISTFILESIZE=1000000
HISTSIZE=1000000
HISTCONTROL=ignoreboth
HISTIGNORE='ls:bg:fg:history'
HISTTIMEFORMAT='%F %T '
shopt -s cmdhist
PROMPT_COMMAND='history -a'

# completion
if [ -f /etc/bash_completion ]; then
 . /etc/bash_completion
fi

# prompt
export GIT_PS1_SHOWDIRTYSTATE=1
__prompt_color="1;32m"
# Alternate color for hostname if the generated color clashes with prompt color
__alternate_color="1;33m"
__hostnamecolor="$__prompt_color"
# If logged in with ssh, pick a color derived from hostname
if [ -n "$SSH_CLIENT" ]; then
  __hostnamecolor="1;$(hostname | od | tr ' ' '\n' | awk '{total = total + $1}END{print 30 + (total % 6)}')m"
  # Fixup color clash
  if [ "$__hostnamecolor" = "$__prompt_color" ]; then
    __hostnamecolor="$__alternate_color"
  fi
fi
__red="1;31m"
PS1='\n$(ret=$?; test $ret -ne 0 && printf "\[\e[$__red\]$ret\[\e[0m\] ")\[\e[$__prompt_color\]\u@\[\e[$__hostnamecolor\]\h \[\e[$__prompt_color\]\w$(__git_ps1 " [git:%s]")\[\e[0m\]\n$ '
