# uncomment below to figure out bottlenecks
# zmodload zsh/zprof

# this is needed for gpg sign to work
export GPG_TTY=$(tty)

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if [[ "$OSTYPE" == darwin* ]]; then
    ssh-add --apple-load-keychain -q
    # ^ will hopefull get integrated into prezto and can be removed from here
fi

# load all common vars, aliases, functions, etc
source $HOME/.env_common
source $HOME/.env_private
source $HOME/.alias_common
source $HOME/.functions_common
source $HOME/.zshpaths
source $HOME/.zsh_custom_pre

# load zsh plugins
antidote_dir=~/.antidote
plugins_txt=~/.zsh_plugins.txt
static_file=~/.zsh_plugins.zsh

# Clone antidote if necessary and generate a static plugin file.
if [[ ! $static_file -nt $plugins_txt ]]; then
  [[ -e $antidote_dir ]] ||
    git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
  (
    source $antidote_dir/antidote.zsh
    [[ -e $plugins_txt ]] || touch $plugins_txt
    antidote bundle <$plugins_txt >$static_file
  )
fi

# add antidote commands like `antidote update` in your interactive shell session
autoload -Uz $antidote_dir/functions/antidote

# source the static plugins file
source $static_file

# cleanup
unset antidote_dir plugins_txt static_file

source $HOME/.zsh_custom_post

# setopt autocd extendedglob nomatch notify
# setopt autolist auto_menu

## never ever beep ever
setopt NO_BEEP

# fix paths here again to make sure local bins have precedence
# over pyenv
source $HOME/.fix_path

# make sure bash-only autocompletions are loaded correctly
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

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
