if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

export PS1='\[\033[0;32m\]\u \[\033[0;34m\]\w\[\033[0m\]$(__git_ps1 " (%s)")$(__venv_ps1)\n$ '
export EDITOR="vim"
export HISTCONTROL=ignoreboth
export GOPATH=$HOME/devel/go

PATH=$HOME/devel/go/bin:$PATH
PATH=$HOME/.rvm/bin:$PATH
PATH=$HOME/.local/bin:$PATH
PATH=$HOME/bin:$PATH
export PATH

export VIRTUAL_ENV_DISABLE_PROMPT=1
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export WORKON_HOME=~/.virtualenvs

alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -lha'
alias dc='docker-compose'
alias dm='docker-machine'
alias git='hub'
alias vpn='sudo openconnect -q -b vpn.cybera.ca'
alias vpn-off='sudo killall openconnect'

eval "$(dircolors)"

[ -f ~/.xmodmap ] && xmodmap ~/.xmodmap >/dev/null 2>&1
[ -f ~/.rvm/scripts/rvm ] && . ~/.rvm/scripts/rvm
[ -f ~/.fzf.bash ] && . ~/.fzf.bash
[ -f ~/.local/bin/virtualenvwrapper.sh ] && . ~/.local/bin/virtualenvwrapper.sh

dmswitch() {
    eval "$(docker-machine env $1)"
}

_dmswitch() {
    local cur=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$(docker-machine ls -q)" -- $cur) )
}

complete -F _activate activate
complete -F _dmswitch dmswitch

__venv_ps1() {
    if [ ! -z ${VIRTUAL_ENV+x} ]; then
        echo " (py:$(basename "$VIRTUAL_ENV"))"
    fi
}
