# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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
    $(brew --prefix)/share/zsh/site-functions
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

# had to move this out of zsh_plugins because it made zsh crash
antibody bundle robbyrussell/oh-my-zsh path:plugins/aws

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

if [[ $commands[direnv] ]]; then
    eval "$(direnv hook zsh)"
fi
# uncomment below to figure out bottlenecks
# zprof

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
