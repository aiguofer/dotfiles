#!/bin/bash
COMPLETIONS_DIR=~/.zsh/completions

# Generate completion for apps that generate them
completion_generators=(
    "poetry completions zsh"
    "oc completion zsh"
    "kubectl completion zsh"
    "openshift completion zsh"
    "s2i completion zsh"
    "heroku autocomplete:script zsh"
)

for completion_generator in "${completion_generators[@]}"; do
    cmd=$(echo $completion_generator | cut -d' ' -f1)
    if hash $cmd 2>/dev/null; then
        eval "$completion_generator" > $COMPLETIONS_DIR/_$cmd
    fi
done

# source ~/.bash_completion.d/python-argcomplete.sh

# Download completions from github
download_completions=(
    "jupyter/jupyter_core/master/examples/completions-zsh _jupyter"
    "docker/cli/master/contrib/completion/zsh/_docker _docker"
    "docker/compose/master/contrib/completion/zsh/_docker-compose _docker-compose"
    "robbyrussell/oh-my-zsh/master/plugins/geeknote/_geeknote _geeknote"
    # "janernsting/dotfiles/master/zsh/custom/plugins/tlp/_tlp _tlp"
    # "janernsting/dotfiles/master/zsh/custom/plugins/tlp/_tlp-radio-devices _tlp-radio-devices"
    # "janernsting/dotfiles/master/zsh/custom/plugins/tlp/_tlp-stat _tlp-stat"
)
base_url="https://raw.githubusercontent.com"
for comp in "${download_completions[@]}"; do
    IFS=' ' read -r -a parts <<< "$comp"
    curl -sL "${base_url}/${parts[0]}" -o "$COMPLETIONS_DIR/${parts[1]}"
done

# Generate completions for Click apps
click_apps=(
    black
    pip-compile
    pip-sync
    git-up
    cookiecutter
)

for app in "${click_apps[@]}";do
    env_magic=$(echo $app | tr '[:lower:]' '[:upper:]' | tr '-' '_')
    eval "_${env_magic}_COMPLETE=source_zsh $app > $COMPLETIONS_DIR/_$app"
done

# make sure #compdef is always at the top of completions
for f in $(find $COMPLETIONS_DIR -type f); do
    cname=$(echo $f | sed -E 's/.+_//')
    header="#compdef $cname"

    if [ -z "$(grep "$header" $f)" ] && [ "$cname" != "tlp-radio-devices" ]; then
        sed -i "1i$header" $f
    fi
done

# clear existing comp cache
rm -rf ~/.zcomp*
