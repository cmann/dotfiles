export PS1='\[\033[0;32m\]\u \[\033[0;34m\]\w\[\033[0m\] $(__git_ps1 "(%s)")\n$ '
export EDITOR="vim"
export HISTCONTROL=ignoreboth
export GOPATH=$HOME/devel/go
export VAGRANT_DEFAULT_PROVIDER=virtualbox

PATH=$HOME/devel/go/bin:$PATH
PATH=$HOME/bin:$PATH

alias ll='ls -lh'
alias la='ls -lha'
alias dm='docker-machine'
alias dockerrm='docker rm $(docker ps -a | grep -v Up | grep -v data | grep -v CONTAINER | cut -d" " -f1) 2>/dev/null'
alias dockerrmi='docker rmi $(docker images -q -f dangling=true) 2>/dev/null'
alias dockerrmv='docker volume rm $(docker volume ls -q -f dangling=true) 2>/dev/null'
alias dockerclean='dockerrm; dockerrmi'
alias git='hub'
alias dssh='ssh -i $DEFAULT_SSH_KEY'

eval "$($HOME/devel/lmc/bin/lmc init -)"

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
    xmodmap ~/.xmodmap
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

vpnon() {
    ps aux | grep openconnect | grep -v grep > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        echo "VPN already connected"
        return
    fi

    case $OSTYPE in
    darwin*)
        user=$(security 2>/dev/null find-generic-password -gs vpn.cybera.ca | grep "acct" | cut -d '"' -f 4 | sed 's/\\$/\\\\\\\$/g')
        password=$(security 2>&1 >/dev/null find-generic-password -gs vpn.cybera.ca | cut -d '"' -f 2 | sed 's/\\$/\\\\\\\$/g')
        if [ "$password" == "security: SecKeychainSearchCopyNext: The specified item could not be found in the keychain." ]; then
            sudo openconnect -q -l -b vpn.cybera.ca
        else
            echo "$password" | sudo openconnect -q -l -u $user --passwd-on-stdin -b vpn.cybera.ca
        fi
        ;;
    linux*)
        sudo openconnect -q -b vpn.cybera.ca
        ;;
    esac
}

vpnoff() {
    sudo killall openconnect
}
