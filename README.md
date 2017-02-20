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
sudo npm install -g tern js-beautify vmd eslint eslint-plugin-angular
```
