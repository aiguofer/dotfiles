#!/bin/sh

git_installers=(
    getantibody/installer/master/install # antibody
    creationix/nvm/v0.33.11/install.sh   # nvm
    pyenv/pyenv-installer/master/bin/pyenv-installer # pyenv
)

for installer in $git_installers; do
    curl -sL https://raw.githubusercontent.com/$installer | bash
done

# instal pipx and python executables
curl -sL https://raw.githubusercontent.com/cs01/pipx/master/get-pipx.py | PYENV_VERSION=system python3

for pkg in $(cat ./requirements_pipx.txt);do
    pipx install $pkg
done

# get zsh completions
./generate_completions.sh
