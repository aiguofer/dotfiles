#!/bin/bash

# install https://gitlab.com/wavexx/acpilight

git_installers=(
    getantibody/installer/master/install # antibody
    creationix/nvm/v0.33.11/install.sh   # nvm
    pyenv/pyenv-installer/master/bin/pyenv-installer # pyenv
)

for installer in "${git_installers[@]}"; do
    curl -sL https://raw.githubusercontent.com/$installer | bash
done

# install pipx and python executables
PYENV_VERSION=system pip3 install --user pipx

for pkg in $(cat ./requirements_pipx.txt);do
    pipx install $pkg
done

necessary_packages=(
    emacs
    git
    curl
    python2
    zsh
    powerline
    rofi
    i3 # gaps or regolith
    i3lock-color # https://github.com/codejamninja/i3lock-color-ubuntu
    polybar
    shutter
    silver-searcher # ag
    autorandr
    playerctl
    dunst
    tmux
    terminator
    redshift-gtk
    insync
    google-play-music-desktop-player
)

wanted_packages=(
    todoist
    upwork
    rescuetime
    franz
)

# deps for building compton
# sudo apt install meson libev4 libev libev-dev xcb-dev x11-xcb libx11-xcb libx11-xcb-dev libxcb-damage0-dev libxcb-sync-dev libxcb-xinerama0-dev libxcb-present-dev uthash-dev libconfig-dev libxdg-basedir-dev gl-dev libglc-dev
