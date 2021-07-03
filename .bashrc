# path

EDITOR="vim"

export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.npm-packages/bin:$PATH"
export PATH="$HOME/.local/share/git-plus/:$PATH"
export PATH="$HOME/jpm/bin/:$PATH"

export WS="$HOME/development/projects/o2/portal/liferay7-poc/workspace/bundles/osgi/modules"
export LUK="$HOME/development/projects/lukreo/liferay/bundles/osgi/modules"

# history
HISTSIZE=1000000
HISTFILESIZE=$HISTSIZE
HISTCONTROL=ignoredups:erasedups
shopt -s histappend
function historymerge {
    history -n; history -w; history -c; history -r;
}
trap historymerge EXIT
PROMPT_COMMAND="history -a; ${PROMPT_COMMAND}"

# if this is interactive shell, then bind hh to Ctrl-r (for Vi mode check doc)
if [[ $- =~ .*i.* ]]; then bind '"\C-r": "hstr -- \C-j"'; fi
# if this is interactive shell, then bind 'kill last command' to Ctrl-x k
if [[ $- =~ .*i.* ]]; then bind '"\C-xk": "hstr -k \C-j"'; fi

# completion
if [ -f /etc/bash_completion ]; then
 . /etc/bash_completion
fi

source ~/bin/z.sh

# prompt
export HH_CONFIG=hicolor         # get more colors
icon_directory="📁"
icon_start="┌"
icon_prompt_arrow="➤"
prompt_bottom_line="$(if [ ! -z "$SHELL_NAME" ]; then printf "└🐚 ${SHELL_NAME}${icon_prompt_arrow} "; else printf "└${icon_prompt_arrow} "; fi)"
icon_branch="🌿"
icon_fail="⛈"

function normal {
  printf "\e[0m"
}

function bold {
  printf "\e[1m"
}
function red {
  printf "\e[1;31m"
}

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
PS1='${icon_start}$(ret=$?; test $ret -ne 0 && red && printf " $icon_fail $ret" && normal)${icon_directory} \w$(__git_ps1 " ${icon_branch} %s")\n${prompt_bottom_line}'

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

# navigation aliases
alias ..='cd ../'
alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../../'
alias e='exit'

# some more ls aliases
# alias ll='ls -CFl'
# alias lla='ls -CFal'
# alias l='ls -CF'
# alias la='ls -CFA'
alias exadefault='exa --git --header --grid --group'
alias ll='exadefault -l'
alias lla='exadefault -al'
alias l='exadefault'
alias la='exadefault -a'

# git aliases
alias commit-merge='git commit -e -F "$(git rev-parse --git-dir)/MERGE_MSG"'
alias bc='bcompare'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=critical -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias f='cd $(fd --type d --hidden --exclude .git --exclude node_module --exclude .cache --exclude .npm --exclude .mozilla --exclude .meteor --exclude .nv | fzf)'

# run ssh agent
# eval `ssh-agent -s`

# add ripgrep configuration
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

# fzf loves ripgrep
export FZF_DEFAULT_COMMAND='rg --files --no-ignore-vcs --hidden'

function check_gzip_compression {
  curl -s -I -H 'Accept-Encoding: gzip' $1 |grep -i "Content-Encoding"
}

function check_compression {
  curl -s -I -H 'Accept-Encoding: br,gzip,deflate' $1 |grep -i "Content-Encoding"
}

if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg --files'
  export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi

# completions
complete -W "convert create deploy extension gw help open samples server sh update upgradeProps version watch" blade

# direnv hook https://direnv.net/docs/hook.html
if [ $(command -v direnv) ]; then
  eval "$(direnv hook bash)"
fi
