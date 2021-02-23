if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

export PS1='\[\033[0;32m\]\u \[\033[0;34m\]\w\[\033[0m\]$(__git_ps1 " (%s)")$(__venv_ps1)\n$ '
export EDITOR="nvim"
export HISTCONTROL=ignoreboth
export GOPATH=$HOME/devel/go

PATH=/var/lib/snapd/snap/bin:$PATH
PATH=$HOME/devel/go/bin:$PATH
PATH=$HOME/.rvm/bin:$PATH
PATH=$HOME/.local/bin:$PATH
PATH=$HOME/bin:$PATH
PATH=$HOME/.nimble/bin:$PATH
PATH=$HOME/.cargo/bin:$PATH
PATH=$HOME/.yarn/bin:$PATH
PATH=$HOME/.nimble/bin:$PATH
PATH=$HOME/.poetry/bin:$PATH
export PATH

export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS'
--color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
--color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
'

alias ls='ls --color=auto'
alias ll='ls -lh'
alias la='ls -lha'
alias dc='docker-compose'
alias dm='docker-machine'
alias vim='nvim'

alias git='hub'
alias gs='git status'
alias gl='git log --oneline'
alias gri='git rebase -i --autosquash --onto $(git merge-base -a HEAD @{upstream})'

if [ -f ~/.dir_colors ]; then
    eval "$(dircolors ~/.dir_colors)"
else
    eval "$(dircolors)"
fi

[ -f ~/.xmodmap ] && xmodmap ~/.xmodmap >/dev/null 2>&1
[ -f ~/.rvm/scripts/rvm ] && . ~/.rvm/scripts/rvm
[ -f /usr/share/fzf/shell/key-bindings.bash ] && . /usr/share/fzf/shell/key-bindings.bash
[ -f /usr/share/git-core/contrib/completion/git-prompt.sh ] && . /usr/share/git-core/contrib/completion/git-prompt.sh

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

vterm_printf() {
    if [ -n "$TMUX" ]; then
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
export PS1=$PS1'\[$(vterm_prompt_end)\]'
