#!/bin/bash

# setup python environment
pyenv virtualenv --system-site-packages -p /usr/bin/python3 default

pyenv global default

pip3 install -r packages/python_global.txt
