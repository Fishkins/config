#!/usr/bin/env bash
# Get latest packages
brew update; brew upgrade
 
# core
brew install coreutils
 
# key commands
brew install \
  binutils \
  diffutils \
  findutils \
  gawk \
  gnu-indent \
  gnu-sed \
  gnu-tar \
  gnu-which \
  gnutls \
  grep \
  gzip \
  screen \
  watch \
  wdiff \
  wget
# OS X ships a GNU version, but too old
brew install \
  bash \
  m4 \
  make \
  zsh

brew install curl aspell jq

brew install homebrew/cask/emacs
# "Quick Start" https://github.com/syl20bnr/spacemacs
