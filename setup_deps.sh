#!/bin/bash

# Install homebrew
if [[ "$OSTYPE" == linux* ]]; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval $($HOME/.linuxbrew/bin/brew shellenv)
else
    softwareupdate --install xcode-select

    xcode-select --install

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    eval $(/opt/homebrew/bin/brew shellenv)
fi

# Install homebrew deps
while read tap; do
    brew tap $tap
done < packages/homebrew_taps.txt

while read pkg; do
    brew install $pkg
done < packages/homebrew_common.txt

if [[ "$OSTYPE" == darwin* ]]; then
    while read pkg; do
        brew install $pkg
    done < packages/homebrew_mac.txt

    while read pkg; do
        brew install --cask $pkg
    done < packages/homebrew_cask.txt
fi

# Install version managers
git_installers=(
    creationix/nvm/v0.39.2/install.sh   # nvm
    pyenv/pyenv-installer/master/bin/pyenv-installer # pyenv
    rbenv/rbenv-installer/master/bin/rbenv-installer # rbenv
)

for installer in "${git_installers[@]}"; do
    curl -sL https://raw.githubusercontent.com/$installer | bash
done

# install extra pyenv plugins
pyenv_plugins=(
    aiguofer/pyenv-version-alias
    jawshooah/pyenv-default-packages
    aiguofer/pyenv-jupyter-kernel
)

for plugin in "${pyenv_plugins[@]}"; do
    plugin_name=$(echo $plugin | cut -d '/' -f2)
    git clone https://github.com/$plugin $(pyenv root)/plugins/$plugin_name
done

while read pkg; do
    pipx install $pkg
done < packages/pipx.txt

# install nix installer, needed for xidlehooks on linux
if [[ "$OSTYPE" == linux* ]]; then
    sh <(curl -L https://nixos.org/nix/install) --daemon

    while read pkg; do
        nix-env -iA $pkg
    done < packages/nix.txt
fi


necessary_packages_common=(
    emacs
    git
    curl
    wget
    zsh
    insync
    google-chrome
    1password
)

necessary_packages_linux=(
    zathura # pdf viewer
    gthumb # image viewer
    nemo # file manager
    insync-nemo
    spotify # music player
    xbacklight # https://gitlab.com/wavexx/acpilight # manage backlight
    feh # image viewere, set background
    picom # compositor, previously compton
    earlyoom # prevent system from locking up due to OOM
    pasystray # pulse audio tray
    nm-applet # networkmanager tray
    qalculate-gtk # calculator
    lightdm-gtk-greeter # desktop manager
    lightdm # destkop manager
    alacritty
    redshift-gtk
    autorandr
    arandr
    automirror # https://github.com/schlomo/automirror/
    playerctl
    dunst
    i3-gaps # gaps or regolith
    i3lock-color # https://github.com/codejamninja/i3lock-color-ubuntu
    polybar
    shutter
    rofi
)

wanted_packages=(
    todoist
    upwork
    rescuetime
    franz
    tig
    htop
)

# deps for building compton
# sudo apt install meson libev4 libev libev-dev xcb-dev x11-xcb libx11-xcb libx11-xcb-dev libxcb-damage0-dev libxcb-sync-dev libxcb-xinerama0-dev libxcb-present-dev uthash-dev libconfig-dev libxdg-basedir-dev gl-dev libglc-dev
