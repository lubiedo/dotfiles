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

# install user bins
[ ! -d ~/bin ] && mkdir ~/bin
for f in bin/* ;do
  ln -s $(pwd)/${f} ~/${f}
done

# install vim plugins && extras
[ ! -e ~/.vim/bundle/Vundle.vim ] && git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
(
  cd ~/.vim/bundle/Vundle.vim && git pull
)
vim -c ':PluginInstall' -c ':qall'
cp -r ./vim/* ~/.vim/
download "https://raw.githubusercontent.com/s3rvac/vim-syntax-yara/master/syntax/yara.vim"  ~/.vim/syntax/yara.vim

# brew installs
pkgs=(
  exa   # better ls
  up    # visual command pipeliner
)
for pkg in ${pkgs[*]};do
  brew install $pkg
done

