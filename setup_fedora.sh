script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# mkdir ~/bin

sudo dnf -y install dnf-plugins-core
sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
sudo dnf -y install docker-ce docker-ce-cli containerd.io neovim hub xclip
python3 -m pip install --user --upgrade pynvim
sudo systemctl start docker
sudo usermod -aG docker $USER

sudo dnf -y install libtool aspell

curl -L "https://github.com/docker/compose/releases/download/1.23.2/docker-compose-$(uname -s)-$(uname -m)" -o ~/bin/docker-compose
curl -L "https://github.com/docker/machine/releases/download/v0.16.0/docker-machine-$(uname -s)-$(uname -m)" -o ~/bin/docker-machine

# sudo ln -s /usr/share/git-core/contrib/completion/git-prompt.sh /etc/profile.d
# sudo ln -s $script_dir/fonts/conf.d/10-bci-hint.conf /etc/fonts/conf.d
# sudo ln -s /usr/share/fontconfig/conf.avail/10-sub-pixel-rgb.conf /etc/fonts/conf.d
# sudo ln -s /usr/share/fontconfig/conf.avail/11-lcdfilter-default.conf /etc/fonts/conf.d
