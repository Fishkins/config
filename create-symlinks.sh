#!/usr/bin/env zsh
for file in aspell.en.pws gitconfig gitignore_global ideavimrc ispell_english profile psqlrc spacemacs vimrc zprofile zsh zshrc
do
   rm .$file
   ln -s ~/Config/$file ~/.$file
done
