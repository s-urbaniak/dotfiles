#!/bin/sh

PLATFORM=$(uname)
PWD=$(pwd)

if [ "$PLATFORM" == 'Darwin' ]
then
    ln -s "$PWD"/bash/bash_profile ~/.bash_profile
fi

if [ "$PLATFORM" == 'Linux' ]
then
    ln -s "$PWD"/bash/bash_profile ~/.bashrc
    mkdir -p ~/.config/gtk-3.0 && ln -s "$PWD/config/gtk-3.0/gtk.css" ~/.config/gtk-3.0/gtk.css
fi

ln -s "$PWD"/tmux.conf ~/.tmux.conf
ln -s "$PWD"/vim/vimrc ~/.vimrc
ln -s "$PWD"/git/gitconfig ~/.gitconfig
ln -s "$PWD"/plan9/lib ~/lib

