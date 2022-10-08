#!/bin/zsh

if [[ $commands[brew] ]]; then
    echo "Updating Homebrew packages"
    brew upgrade
    if [[ "$OSTYPE" == darwin* ]]; then
        brew cask upgrade
    fi
fi

if [[ $commands[antibody] ]]; then
    echo "Updating zsh modules"
    antibody update
fi

if [[ $commands[pyenv] ]]; then
    echo "Updating pyenv"
    pyenv update

    pyenv versions --bare \
        | grep -v "/" \
        | xargs -L 1 -I {} bash -c "PYENV_VERSION={} pip install -U pip;PYENV_VERSION={} pip install -U -r ~/.pyenv/default-packages"

fi

if [[ $commands[nvm] ]]; then
    echo "Updating nvm"
    nvm upgrade
fi

if [[ $commands[rbenv] ]]; then
    echo "Updating rbenv"
    cd $HOME/.rbenv/
    git pull
fi

tpm=$HOME/.tmux/plugins/tpm/bin/update_plugins
if [[ -f $tpm ]]; then
    echo "Updating tmux plugins"
    $tpm all
fi

if [[ $commands[pipx] ]]; then
    echo "Updating packages installed through pipx"
    pipx upgrade-all
fi

if [[ $commands[nix-env] ]]; then
    echo "Updating packages installed through nix-env"
    nix-env -u
fi

if [[ $commands[dnf] ]]; then
    echo "Updating Fedora packages"
    sudo dnf -y upgrade --refresh
fi

if [[ $commands[apt] && ! -L $(which apt) ]]; then
    echo "Updating Ubuntu packages"
    sudo apt update
    sudo apt full-upgrade -y
fi

if [[ $commands[flatpak] ]]; then
    echo "Updating flatpack packages"
    flatpak -y update
fi

if [[ $commands[snap] ]]; then
    echo "Updating snap packages"
    sudo snap refresh
fi

if [[ $commands[fwupdmgr] ]]; then
    echo "Updating firmware"
    sudo fwupdmgr refresh
    sudo fwupdmgr update
fi

echo "Updating completion scripts"
./setup_completions.sh
