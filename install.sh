#!/bin/bash

PLATFORM=$(uname)
PWD=$(pwd)

if [ "$PLATFORM" == 'Darwin' ]
then
    ln -s "$PWD"/bash/bash_profile ~/.bash_profile
    ln -s "$PWD"/tmux/tmux.osx.conf ~/.tmux.conf
fi

if [ "$PLATFORM" == 'Linux' ]
then
    ln -s "$PWD"/bash/bash_profile ~/.bashrc
    ln -s "$PWD"/tmux/tmux.linux.conf ~/.tmux.conf
fi

ln -s "$PWD"/vim/vimrc ~/.vimrc
ln -s "$PWD"/git/gitconfig ~/.gitconfig

