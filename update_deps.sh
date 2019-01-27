#!/bin/bash

if [ $commands[antibody] ]; then
    antibody update
fi
if [ $commands[pyenv] ]; then
    pyenv update
fi

~/.tmux/plugins/tpm/bin/update_plugins all

if [ $commands[pipx] ]; then
    pipx upgrade-all
fi

if [ $commands[dnf] ]; then
    sudo dnf -y upgrade --refresh
fi

if [ $commands[flatpak] ]; then
    flatpak -y update
fi

./generate_completions.sh
