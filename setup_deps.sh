#!/bin/bash

git_installers=(
    creationix/nvm/v0.35.2/install.sh   # nvm
    pyenv/pyenv-installer/master/bin/pyenv-installer # pyenv
    rbenv/rbenv-installer/raw/master/bin/rbenv-installer # rbenv
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

# Install homebrew
if [[ "$OSTYPE" == linux* ]]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
    eval $($HOME/.linuxbrew/bin/brew shellenv)
else
    softwareupdate --install xcode-select

    xcode-select --install

    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

    while read pkg; do
        brew install $pkg
    done < packages/homebrew_mac.txt

    while read pkg; do
        brew cask install $pkg
    done < packages/homebrew_cask.txt
fi

while read pkg; do
    brew install $pkg
done < packages/homebrew_common.txt

while read pkg; do
    pipx install $pkg
done < packages/pipx.txt

necessary_packages_common=(
    emacs
    git
    curl
    wget
    zsh
    insync
    google-chrome
)

necessary_packages_linux=(
    zathura # pdf viewer
    gthumb # image viewer
    nemo # file manager
    insync-nemo
    google-play-music-desktop-player # music player
    xbacklight # https://gitlab.com/wavexx/acpilight # manage backlight
    feh # image viewere, set background
    compton # compositor
    earlyoom # prevent system from locking up due to OOM
    pasystray # pulse audio tray
    nm-applet # networkmanager tray
    qalculate-gtk # calculator
    lightdm-gtk-greeter # desktop manager
    lightdm # destkop manager
    terminator
    redshift-gtk
    autorandr
    arandr
    automirror # https://github.com/schlomo/automirror/
    playerctl
    dunst
    i3 # gaps or regolith
    i3lock-color # https://github.com/codejamninja/i3lock-color-ubuntu
    polybar
    shutter
    rofi
    python2
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
