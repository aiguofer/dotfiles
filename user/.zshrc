#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# # Source Prezto.
# if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
#   source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
# fi

# theme customizations
export POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(status virtualenv context dir vcs)
export POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=()
export POWERLEVEL9K_VIRTUALENV_BACKGROUND='blue'
export VIRTUAL_ENV_DISABLE_PROMPT=1
export TERM=xterm-256color

export NVM_LAZY_LOAD=true
export NVM_AUTO_USE=true

# antibody
source <(antibody init)
antibody bundle < ~/.zsh_plugins.txt

# some of these must be loaded after the extension so these can't be in zshenv
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=3"

# custom functions
pip_install_save() {
    package_name=$1
    requirements_file=$2
    if [[ -z $requirements_file ]]
    then
        requirements_file='./requirements.txt'
    fi

    pip install -U $package_name
    package_entry=$(pip freeze | grep -i "$package_name==")
    search_package="$package_name([><=~]|$).*"

    if [[ $(grep -E -i $search_package $requirements_file) ]]
    then
        sed -E -i "s/$search_package/$package_entry/" $requirements_file
    else
        echo $package_entry >> $requirements_file
    fi
}

# common
alias mkdir='mkdir -pv'
alias mount='mount |column -t'
# do not delete / or prompt if deleting more than 3 files at a time #
alias rm='rm -I --preserve-root'

# confirmation #
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'

# Parenting changing perms on / #
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

alias free='free -lth'

## get top process eating memory
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'

## get top process eating cpu ##
alias pscpu='ps auxf | sort -nr -k 3'
alias pscpu10='ps auxf | sort -nr -k 3 | head -10'

alias psgrep='ps -ef | grep'

alias wget='wget -c'

# setopt autocd extendedglob nomatch notify
# setopt autolist auto_menu

## never ever beep ever
setopt NO_BEEP

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the list of directories that cd searches.
cdpath=(
    $HOME
    $HOME/Projects
    $cdpath
)

# Set the list of directories that Zsh searches for functions.
fpath=(~/.zsh/completions $fpath)

# fix path here to make sure local bins have precedence
# over pyenv
path=(
    $HOME{,/.local}/bin
    /usr/local/{bin,sbin}
    $path
)

# additonal autocompletion
autoload -U bashcompinit && bashcompinit

