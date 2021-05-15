#!/usr/bin/env bash

dnf install -y \
    fedora-workstation-repositories \

dnf config-manager --set-enabled google-chrome

dnf install -y \
    stow \
    google-chrome-stable \
    emacs \
    tmux \
    fzf \

curl -fsSL https://get.docker.com | sh -
groupadd docker
usermod -aG docker $SUDO_USER
