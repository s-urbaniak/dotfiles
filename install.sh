#!/bin/bash

PLATFORM=$(uname)
PWD=$(pwd)

if [ "$PLATFORM" == 'Darwin' ]
then
    ln -sf "$PWD"/bash/bash_profile ~/.bash_profile
fi

if [ "$PLATFORM" == 'Linux' ]
then
    ln -sf "$PWD"/bash/bash_profile ~/.bashrc
    ln -sf "$PWD"/profile ~/.profile
    mkdir -p ~/.config/gtk-3.0 && ln -sf "$PWD/config/gtk-3.0/gtk.css" ~/.config/gtk-3.0/gtk.css
    mkdir -p ~/.themes && ln -s "$PWD/themes/small" ~/.themes/small
fi

ln -sf "$PWD"/tmux.conf ~/.tmux.conf
ln -sf "$PWD"/vim/vimrc ~/.vimrc
ln -sf "$PWD"/git/gitconfig ~/.gitconfig

mkdir -p ~/.cache/emacs/elpa
ln -sf $PWD/emacs.d ~/.emacs.d
touch ~/.emacs.d/local.el
touch ~/.emacs.d/custom.el

