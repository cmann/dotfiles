#!/usr/bin/env bash

set -eu

dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

cd "$dir"

sudo dnf install -y \
     fedora-workstation-repositories

sudo dnf config-manager --set-enabled google-chrome

sudo dnf install -y \
     stow \
     google-chrome-stable \
     emacs \
     tmux \
     fzf \
     docker-compose \
     ShellCheck \
     nodejs \
     golang

sudo npm install --global yarn

curl -fsSL https://get.docker.com | sudo sh -
sudo groupadd docker
sudo usermod -aG docker $USER

stow bash
stow emacs
stow tmux
stow git

source ~/.bashrc

curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python3 -
source ~/.poetry/env
poetry config virtualenvs.in-project true

mkdir -p ~/devel/go
go get golang.org/x/tools/cmd/goimports
go get golang.org/x/tools/cmd/godoc
go get github.com/rogpeppe/godef
