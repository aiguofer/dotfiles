# .emacs.d
My emacs config

## Setting up

Clone repo in home directory

```bash
cd ~
git clone --recursive https://github.com/aiguofer/.emacs.d.git
```

Install pyenv to manage python environments

```bash
curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
```

Install python dependencies for `elpy`. This will need to be done in each `pyenv` environment

```bash
pip install -U jedi flake8 importmagic autopep8 yapf elpy
```

Install javascript dependencies

```bash
npm install -g tide prettier js-beautify vmd eslint eslint-plugin-angular
```

Install necessary external tools

```bash
pip install -U black isort pyment
```

Install `ag`, see https://github.com/ggreer/the_silver_searcher#installing
