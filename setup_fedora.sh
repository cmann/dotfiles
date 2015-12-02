script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

sudo ln -s /usr/share/git-core/contrib/completion/git-prompt.sh /etc/profile.d

sudo ln -s $script_dir/fonts/conf.d/10-bci-hint.conf /etc/fonts/conf.d
sudo ln -s /usr/share/fontconfig/conf.avail/10-sub-pixel-rgb.conf /etc/fonts/conf.d
sudo ln -s /usr/share/fontconfig/conf.avail/11-lcdfilter-default.conf /etc/fonts/conf.d
