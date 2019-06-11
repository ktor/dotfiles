# path
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.npm-packages/bin:$PATH"
export PATH="$HOME/development/maven/apache-maven-3.6.0/bin:$PATH"
export PATH="$HOME/.local/share/git-plus/:$PATH"

EDITOR="vim"

# history
export HISTFILESIZE=-1
export HISTSIZE=-1
export HISTCONTROL=ignoreboth:erasedups
export HISTIGNORE='ls:bg:fg:history:ps:top:cd:exit:man'
export HISTTIMEFORMAT='%F %T '
shopt -s histappend
export PROMPT_COMMAND="history -a; history -r; $PROMPT_COMMAND"
export HH_CONFIG=hicolor         # get more colors
export PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND}"   # mem/file sync
# if this is interactive shell, then bind hh to Ctrl-r (for Vi mode check doc)
if [[ $- =~ .*i.* ]]; then bind '"\C-r": "hstr -- \C-j"'; fi
# if this is interactive shell, then bind 'kill last command' to Ctrl-x k
if [[ $- =~ .*i.* ]]; then bind '"\C-xk": "hstr -k \C-j"'; fi

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

# enable color support of ls and also add handy aliases
if [ $(command -v dircolors) ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

if [ $(command -v bat) ]; then
  alias bat='bat --theme GitHub'
fi
if [ $(command -v highlight) ]; then
  # Pipe Highlight to less
  # export LESSOPEN="| $(which highlight) %s --out-format xterm256 --line-numbers --quiet --force --style solarized-light"
  # export LESS=" -R"
  alias lesscolor='less -m -N -g -i -J --line-numbers --underline-special'
  alias more='less'

  # Use "highlight" in place of "cat"
  alias catcolor="highlight $1 --out-format xterm256 --line-numbers --quiet --force --style solarized-light"
  function curljson() {
    curl $1 | python -m json.tool | highlight --out-format xterm256 --syntax json --line-numbers --quiet --force --style github;
  }
fi


# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# git aliases
alias commit-merge='git commit -e -F "$(git rev-parse --git-dir)/MERGE_MSG"'
alias bc='bcompare'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# run ssh agent
# eval `ssh-agent -s`

# add ripgrep configuration
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
