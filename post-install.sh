#!/bin/zsh -f

# install doomemacs
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules
ln -s /opt/homebrew/opt/emacs-mac/Emacs.app /Applications/Emacs.app
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
~/.config/emacs/bin/doom sync

