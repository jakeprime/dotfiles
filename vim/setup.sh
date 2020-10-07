#!/bin/sh

rm ~/.vimrc
ln -s ~/.dotfile/vim/vimrc ~/.vimrc
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
