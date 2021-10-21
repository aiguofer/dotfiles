#!/bin/bash

# setup python environment
pyenv virtualenv --system-site-packages -p /usr/bin/python3 default3

pyenv global default3

pip3 install -r packages/python_global.txt
