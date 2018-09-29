#!/bin/sh

git_installers=(
    getantibody/installer/master/install # antibody
    creationix/nvm/v0.33.11/install.sh   # nvm
    pyenv/pyenv-installer/master/bin/pyenv-installer # pyenv
)

for installer in $git_installers; do
    curl -sL https://raw.githubusercontent.com/$installer | bash
done

# Download Jupyter completions
curl -sL https://raw.githubusercontent.com/jupyter/jupyter_core/master/examples/completions-zsh -o ~/.zsh/completions/_jupyter
