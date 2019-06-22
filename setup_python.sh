#!/bin/bash

# setup python environment
pyenv virtualenv --system-site-packages -p /usr/bin/python2 default2
pyenv virtualenv --system-site-packages -p /usr/bin/python3 default3

pyenv global default3 default2

pip2 install -r requirements.txt
pip3 install -r requirements.txt
