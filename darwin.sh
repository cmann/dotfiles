sudo port install \
     coreutils \
     cmake \
     stow \
     emacs-app \
     git \
     tmux \
     kitty \
     fzf \
     ripgrep \
     fd \
     python310 \
     py310-pip

sudo port select --set python python310
sudo port select --set python3 python310
sudo port select --set pip pip310
sudo port select --set pip3 pip310

stow --no-folding emacs
stow --no-folding tmux
stow --no-folding kitty
stow --no-folding git
stow --no-folding bash
