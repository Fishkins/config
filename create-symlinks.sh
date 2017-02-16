#!/usr/bin/env zsh
for file in aspell.en.pws gitconfig ideavimrc ispell_english profile psqlrc zshrc
do
    rm .$file
    ln -s ~/Config/$file ~/.$file
done
