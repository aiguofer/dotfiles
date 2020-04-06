#!/bin/zsh

if [ $commands[brew] ]; then
    echo "Updating Homebrew packages"
    brew upgrade
    brew cask upgrade
fi

if [ $commands[antibody] ]; then
    echo "Updating zsh modules"
    antibody update
fi

if [ $commands[pyenv] ]; then
    echo "Updating pyenv"
    pyenv update

    pyenv versions --bare \
        | grep -v "/" \
        | xargs -L 1 -I {} bash -c "PYENV_VERSION={} pip install -U pip;PYENV_VERSION={} pip install -U -r ~/.pyenv/default-packages"

fi

if [ $commands[nvm] ]; then
    echo "Updating nvm"
    nvm upgrade
fi

if [ $commands[rbenv] ]; then
    echo "Updating rbenv"
    cd $HOME/.rbenv/
    git pull
fi

tpm=$HOME/.tmux/plugins/tpm/bin/update_plugins
if [ -f $tpm ]; then
    echo "Updating tmux plugins"
    $tpm all
fi

if [ $commands[pipx] ]; then
    echo "Updating packages installed through pipx"
    pipx upgrade-all
fi

if [ $commands[dnf] ]; then
    echo "Updating Fedora packages"
    sudo dnf -y upgrade --refresh
fi

if [ $commands[flatpak] ]; then
    echo "Updating flatpacks"
    flatpak -y update
fi

if [ $commands[apt] ]; then
    echo "Updating Ubuntu packages"
    sudo apt update
    sudo apt full-upgrade -y
fi

echo "Updating completion scripts"
./setup_completions.sh
