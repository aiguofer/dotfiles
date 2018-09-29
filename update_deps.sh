#!/bin/sh

antibody update
pyenv update

~/.tmux/plugins/tpm/bin/update_plugins all

curl -sL https://raw.githubusercontent.com/jupyter/jupyter_core/master/examples/completions-zsh -o user/.zsh/completions/_jupyter
