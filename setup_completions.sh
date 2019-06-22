#!/bin/zsh
COMPLETIONS_DIR=~/.zsh/completions

completion_generators=(
    "poetry completions zsh"
    "oc completion zsh"
    "kubectl completion zsh"
    "s2i completion zsh"
    "heroku autocomplete:script zsh"
)

for completion_generator in "${completion_generators[@]}"; do
    cmd=$(echo $completion_generator | cut -d' ' -f1)
    if [ $commands[$cmd] ]; then
        $completion_generator >! $COMPLETIONS_DIR/_$cmd
    fi
done

# source ~/.bash_completion.d/python-argcomplete.sh

if [ $commands[aws_zsh_completer.sh] ]; then
    ln -s -f $(which aws_zsh_completer.sh) $COMPLETIONS_DIR/_aws
fi

# Download completions from github
# TODO: check versions for these and pull from specific tag. Also, abstract to allow passing in a URL and comand name from array or something similar
curl -sL https://raw.githubusercontent.com/jupyter/jupyter_core/master/examples/completions-zsh -o $COMPLETIONS_DIR/_jupyter

curl -sL https://raw.githubusercontent.com/docker/cli/master/contrib/completion/zsh/_docker -o $COMPLETIONS_DIR/_docker

curl -sL https://raw.githubusercontent.com/docker/compose/master/contrib/completion/zsh/_docker-compose -o $COMPLETIONS_DIR/_docker-compose

curl -sL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/plugins/geeknote/_geeknote -o $COMPLETIONS_DIR/_geeknote
