export PS1='\[\033[0;32m\]\u \[\033[0;34m\]\w\[\033[0m\] $(__git_ps1 "(%s)")\n$ '
export EDITOR="vim"
export HISTCONTROL=ignoreboth
export GOPATH=$HOME/devel/go
export VAGRANT_DEFAULT_PROVIDER=virtualbox

PATH=$HOME/devel/go/bin:$PATH
PATH=$HOME/bin:$PATH

alias dockerrm='docker rm $(docker ps -a | grep -v Up | grep -v data | grep -v CONTAINER | cut -d" " -f1)'
alias dockerrmi='docker rmi $(docker images -q -f dangling=true)'
alias dockerclean='dockerrm; dockerrmi'

eval "$($HOME/devel/lmc/bin/lmc init -)"
eval "$(hub alias -s)"

case $OSTYPE in
darwin*)
    [[ -r /usr/local/etc/bash_completion ]] && . /usr/local/etc/bash_completion
    PATH=/Applications/Postgres.app/Contents/Versions/9.4/bin:$PATH
    PATH=/usr/local/bin:$PATH
    export CLICOLOR=1
    alias tmux="TERM=screen-256color tmux"
    ;;
linux*)
    eval "$(dircolors)"
    alias ls='ls --color=auto'
    ;;
esac

PATH=$HOME/.rvm/bin:$PATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

export PATH

dmswitch() {
    eval "$(docker-machine env $1)"
}
_dmswitch() {
    local cur=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$(docker-machine ls -q)" -- $cur) )
}
complete -F _dmswitch dmswitch
