# .bash_profile - run when starting a login shell

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

export ALTERNATE_EDITOR=""
export SUDO_EDITOR="sudo_editor"
export EDITOR="editor"
export VISUAL="editor"
export PAGER='less'
alias E='${(z)SUDO_EDITOR}'

alias ag='ag -u'
#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi


#
# Temporary Files
#
#
# Set TMPDIR if the variable is not set/empty or the directory doesn't exist
if [[ -z "${TMPDIR}" ]]; then
  export TMPDIR="/tmp/zsh-${UID}"
fi

if [[ ! -d "${TMPDIR}" ]]; then
  mkdir -m 700 "${TMPDIR}"
fi

if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

export JAVA_HOME=/etc/alternatives/java_sdk

# get theme right
export GTK_THEME=Arc-Dark
export XDG_CURRENT_DESKTOP=GNOME
export QT_QPA_PLATFORMTHEME=gtk2

# to prevent Ubuntu from calling compinit, want this done by prezto
export skip_global_compinit=1

# this needs to be here so that it runs on Gnome login
start_systemd &!
