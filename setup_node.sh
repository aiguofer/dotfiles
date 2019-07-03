#!/bin/bash

ln -s $(pwd)/npm-requirements.txt ~/.nvm/default-packages

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

nvm install node
nvm alias default node

# ensure this runs
for pkg in $(cat ./npm-requirements.txt);do
    npm install -g $pkg
done
