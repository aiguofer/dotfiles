#!/bin/bash

# enable emacs keys everywhere
emacs-keys-everywhere

# get zsh completions
./generate_completions.sh

# set up Caps as extra Ctrl key
localectl set-x11-keymap us pc105 "" ctrl:nocaps
