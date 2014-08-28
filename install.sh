#!/bin/bash

PLATFORM=$(uname)

if [ "$PLATFORM" == 'Darwin' ]
then
    ln -s bash/bash_profile ~/.bash_profile
    ln -s tmux/tmux.osx.conf ~/.tmux.conf
fi

if [ "$PLATFORM" == 'Linux' ]
then
    ln -s bash/bash_profile ~/.bashrc
    ln -s tmux/tmux.linux.conf ~/.tmux.conf
fi

ln -s vim/vimrc ~/.vimrc
