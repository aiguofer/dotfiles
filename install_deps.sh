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

# setup python environment
pyenv virtualenv --system-site-packages -p /usr/bin/python2 default2
pyenv virtualenv --system-site-packages -p /usr/bin/python3 default3

pyenv global default3 default2

pip2 install -r requirements.txt
pip3 install -r requirements.txt

# install pipx and python executables
PYENV_VERSION=system pip3 install --user pipx

for pkg in $(cat ./requirements_pipx.txt);do
    pipx install $pkg
done

# enable emacs keys everywhere
emacs-keys-everywhere

# get zsh completions
./generate_completions.sh

# set up Caps as extra Ctrl key
localectl set-x11-keymap us pc105 "" ctrl:nocaps
