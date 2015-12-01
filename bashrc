[[ -r /usr/local/etc/bash_completion ]] && . /usr/local/etc/bash_completion
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

export CLICOLOR=1
export PS1='\[\033[0;32m\]\u \[\033[0;34m\]\w\[\033[0m\] $(__git_ps1 "(%s)")\n$ '
export EDITOR="vim"
export HISTCONTROL=ignoreboth
export GOPATH=$HOME/devel/go
export VAGRANT_DEFAULT_PROVIDER=virtualbox

PATH=/usr/local/bin:$PATH
PATH=$HOME/devel/go/bin:$PATH
PATH=$HOME/.rvm/bin:$PATH
PATH=$HOME/bin:$PATH

alias tmux="TERM=screen-256color tmux"
alias dockerrm='docker rm $(docker ps -a -q)'
alias dockerrmi='docker rmi $(docker images -q -f dangling=true)'
alias dockerclean='dockerrm; dockerrmi'

eval "$($HOME/devel/lmc/bin/lmc init -)"
eval "$(hub alias -s)"

dmswitch() {
    eval "$(docker-machine env $1)"
}
_dmswitch() {
    local cur=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$(docker-machine ls -q)" -- $cur) )
}
complete -F _dmswitch dmswitch

case $OSTYPE in
darwin*)
    PATH=/Applications/Postgres.app/Contents/Versions/9.4/bin:$PATH
    ;;
esac

export PATH
