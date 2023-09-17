#!/usr/bin/env fish

set start (pwd)
cd ~/devel/dotfiles

git stash
git checkout master
git pull origin master

sudo pacman -Syu
fish_update_completions

pacman -Qe > ./arch/pacman_Qe.txt
# This file is only for reference,
# so we know our current versions.

pacman -Qqen > ./arch/pacman_Qqen.txt
# We can use the output to reinstall the same list
# of packages with
# `pacman -S - < ./arch/pacman_Qqen.txt`

git add ./arch/pacman_Qe.txt
git add ./arch/pacman_Qqen.txt

git commit -m "Update arch system."
git push origin master

# Take user back to where they were.
cd $start
