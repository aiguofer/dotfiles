#!/bin/bash

# install fonts
mkdir -p ~/personal
cd personal
git clone https://github.com/ryanoasis/nerd-fonts.git
cd nerd-fonts
./install.sh DejaVuSansMono
./install.sh Hack
