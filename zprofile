# -*- sh -*-
export PATH="/opt/homebrew/bin:/opt/homebrew/opt/curl/bin:/opt/homebrew/bin:$PATH"

# Homebrew paths were generated from the line below. Pre-running since evaluating this line takes a while.
#export PATH="$(brew --prefix openssl)/bin:$(brew --prefix findutils)/libexec/gnubin:$(brew --prefix gnu-indent)/libexec/gnubin:$(brew --prefix gnu-sed)/libexec/gnubin:$(brew --prefix gnu-tar)/libexec/gnubin:$(brew --prefix gnu-which)/libexec/gnubin:$(brew --prefix grep)/libexec/gnubin:$(brew --prefix coreutils)/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/openssl@3/bin:/opt/homebrew/opt/findutils/libexec/gnubin:/opt/homebrew/opt/gnu-indent/libexec/gnubin:/opt/homebrew/opt/gnu-sed/libexec/gnubin:/opt/homebrew/opt/gnu-tar/libexec/gnubin:/opt/homebrew/opt/gnu-which/libexec/gnubin:/opt/homebrew/opt/grep/libexec/gnubin:/opt/homebrew/opt/coreutils/libexec/gnubin:/usr/local/opt/curl/bin:/usr/local/bin:/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/fishkins/.rvm/bin:/usr/local/sbin:/usr/local/opt/ruby/bin"

eval "$(/opt/homebrew/bin/brew shellenv)"
export PATH="/opt/homebrew/opt/python@3.8/libexec/bin:$PATH"

export DEV_HOME=~/Developer
export DEV_BIN=$DEV_HOME/usr/bin
export DEV_SRC=$DEV_HOME/src
export DC_WEB=$DEV_SRC/donorschoose-web/web
export AWS_DEVOPS_DIR=$DEV_SRC/aws-devops
export FASTLY_DIR=$DEV_SRC/fastly
export JMETER_DIR=$DEV_SRC/jmeter
export PATH="$DEV_BIN/jdk-11.0.8+10/Contents/Home/bin:$PATH"
export JAVA_HOME="$HOME/Developer/usr/bin/jdk-11.0.8+10/Contents/Home"

export LESS="IFRX"

