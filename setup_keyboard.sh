#!/bin/bash

# enable emacs keys everywhere
emacs-keys-everywhere

# set up Caps as extra Ctrl key
localectl set-x11-keymap us pc105 "" ctrl:nocaps

# set up terminfo for 24bit color
tic -x screen-24bit.terminfo
tic -x xterm-24bit.terminfo
