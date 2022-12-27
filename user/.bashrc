# .bashrc - run when starting any shell

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

source $HOME/.env_common
source $HOME/.env_private
source $HOME/.alias_common
source $HOME/.functions_common

eval "$(direnv hook bash)"
eval "$(brew shellenv)"
