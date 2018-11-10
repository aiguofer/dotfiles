#!/bin/sh

antibody update
pyenv update
~/.tmux/plugins/tpm/bin/update_plugins all
./generate_completions.sh
