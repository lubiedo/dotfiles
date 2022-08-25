#!/bin/bash

download() {
  URL=$1
  OUT=$2
  [ -n "$OUT" ] && curl -LsSf $URL -o $OUT || curl --remote-header-name -LsSf $URL -O
}

rcs=( tmux.conf vimrc alacritty.yml yabairc zshrc )
for rc in ${rcs[*]};do
  [ ! -L ~/.${rc} ] && ln -s ${PWD}/${rc} ~/.${rc}
done

# install vim plugins && extras
vim -c ':PluginInstall' -c ':qall'
cp -r ./vim/* ~/.vim/
download "https://raw.githubusercontent.com/s3rvac/vim-syntax-yara/master/syntax/yara.vim"  ~/.vim/syntax/yara.vim

