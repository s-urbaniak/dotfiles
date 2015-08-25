#!/bin/sh

PLATFORM=$(uname)
PWD=$(pwd)

if [ "$PLATFORM" == 'Darwin' ]
then
    ln -s "$PWD"/bash/bash_profile ~/.bash_profile
    ln -s "$PWD"/tmux/tmux.osx.conf ~/.tmux.conf
fi

if [ "$PLATFORM" == 'Linux' ]
then
    mkdir -p ~/.config/gtk-3.0 && ln -s "$PWD/config/gtk-3.0/gtk.css" ~/.config/gtk-3.0/gtk.css
    ln -s "$PWD"/bash/bash_profile ~/.bashrc
    ln -s "$PWD"/tmux/tmux.linux.conf ~/.tmux.conf
fi

ln -s "$PWD"/vim/vimrc ~/.vimrc
ln -s "$PWD"/git/gitconfig ~/.gitconfig
ln -s "$PWD"/plan9/lib ~/lib

