#!/bin/bash

download() {
  URL=$1
  OUT=$2
  [ -n "$OUT" ] && curl -LsSf $URL -o $OUT || curl --remote-header-name -LsSf $URL -O
}

rcs=( tmux.conf vimrc config/doom config/kitty yabairc zshrc )
for rc in ${rcs[*]};do
  [ ! -L ~/.${rc} ] && ln -s ${PWD}/${rc} ~/.${rc}
done

# install user bins
[ ! -d ~/bin ] && mkdir ~/bin
for f in bin/* ;do
  ln -s "$(pwd)/${f}" ~/"${f}"
done

# install vim plugins && extras
[ ! -e ~/.vim/bundle/Vundle.vim ] && git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
(
  cd ~/.vim/bundle/Vundle.vim && git pull
)
vim -c ':PluginInstall' -c ':qall'
cp -r "$(pwd)/vim/*" ~/.vim/
download "https://raw.githubusercontent.com/s3rvac/vim-syntax-yara/master/syntax/yara.vim"  ~/.vim/syntax/yara.vim

# brew installs
pkgs=(
  koekeishiya/formulae/yabai  # tiling window manager
  vim       # use brew's vim for python3 support
  up        # visual command pipeliner
  jq        # json processor
  jless     # json's less
  7zip      # 7z
  fzf       # fuzzy search
  rg        # grep replacement
  rclone    # remote drives management
  hexcurse  # ncurses hex editor
  raycast   # replace spotlight
  binutils  # extra binary tools
  fd        # find alternative
  imhex     # binary data explorer
  ffmpeg    # audio/video
  qemu      # virtualization
  gcc       # GNU C compiler
  binwalk   # binary file walker
)
for pkg in ${pkgs[*]};do
  brew install "$pkg"
done

exec bash post-install.sh
