#!/bin/bash

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

nvm install node
nvm alias default node

# ensure this runs
for pkg in $(cat ./packages/npm.txt);do
    npm install -g $pkg
done
