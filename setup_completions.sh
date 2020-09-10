#!/bin/bash
COMPLETIONS_DIR=~/.zsh/completions

# Generate completion for apps that generate them
completion_generators=(
    "poetry completions zsh"
    "pipenv --completion"
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
    "robbyrussell/oh-my-zsh/master/plugins/geeknote/_geeknote _geeknote"
)
base_url="https://raw.githubusercontent.com"
for comp in "${download_completions[@]}"; do
    path=$(echo $comp | cut -d' ' -f1)
    output_name=$(echo $comp | cut -d' ' -f2)

    curl -sL "${base_url}/${path}" -o "$COMPLETIONS_DIR/${output_name}"
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
        if [[ "$OSTYPE" == linux* ]]; then
            params=-i
            sed_cmd="1i$header"
        else
            params="-i ''"
            sed_cmd="1 i\\
$header
"
        fi
        sed $params "$sed_cmd" $f
    fi
done

# clear existing comp cache
rm -rf ~/.zcomp*
