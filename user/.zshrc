#
# Executes commands at the start of an interactive session.
#
# uncomment below to figure out bottlenecks
# zmodload zsh/zprof
source $HOME/.env_common
source $HOME/.alias_common
source $HOME/.functions_common

source $HOME/.zsh_custom_pre

fpath=(
    $HOME/.zsh/completions
    $HOME/.linuxbrew/share/zsh/site-functions
    /home/linuxbrew/.linuxbrew/share/zsh/site-functions
    $fpath
)

# load antibody plugins
source <(antibody init)
antibody bundle < ~/.zsh_plugins.txt

source $HOME/.zsh_custom_post

# setopt autocd extendedglob nomatch notify
# setopt autolist auto_menu

## never ever beep ever
setopt NO_BEEP

# fix paths here again to make sure local bins have precedence
# over pyenv
source $HOME/.zshpaths

# make sure autocompletions are loaded correctly
autoload -Uz bashcompinit && bashcompinit

# Fix for zsh eating scp globs
# see https://unix.stackexchange.com/a/106981/154222
alias scp='noglob scp_wrap'
function scp_wrap {
  local -a args
  local i
  for i in "$@"; do case $i in
    (*:*) args+=($i) ;;
    (*) args+=(${~i}) ;;
  esac; done
  command scp "${(@)args}"
}
# uncomment below to figure out bottlenecks
# zprof
