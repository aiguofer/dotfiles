#!/bin/sh

git_installers=(
    getantibody/installer/master/install # antibody
    creationix/nvm/v0.33.11/install.sh   # nvm
    pyenv/pyenv-installer/master/bin/pyenv-installer # pyenv
)

for installer in $git_installers; do
    curl -sL https://raw.githubusercontent.com/$installer | bash
done

# get zsh completions
./generate_completions.sh
