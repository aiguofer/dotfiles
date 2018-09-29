#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

export JAVA_HOME=/etc/alternatives/java_sdk

# work variables
export PROD=https://api.access.redhat.com
export STAGE=https://api.access.stage.redhat.com
export QA=https://api.access.qa.redhat.com
export CI=https://api.access.devgssci.devlab.phx1.redhat.com
export FTE=https://api.access.devgssfte.devlab.phx1.redhat.com
export RHUSER=rhn-support-difernan
export DEFAULT_USER=difernan

fpath=("$HOME/.zsh/completions" $fpath)
