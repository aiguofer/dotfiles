#!/bin/sh

antibody update
pyenv update
~/.tmux/plugins/tpm/bin/update_plugins all
pipx upgrade-all
./generate_completions.sh
