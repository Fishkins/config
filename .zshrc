PROMPT='$ '
settitle() { printf "\e]0;$@\a" }
dir_in_title() { settitle $PWD }
chpwd_functions=(dir_in_title)
autoload -U select-word-style
select-word-style bash
autoload -U compinit
compinit
setopt completeinword
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
autoload zmv
alias psgrep='ps -ef | head -1;ps -ef | grep -v grep | grep -i'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias ltr='ls -ltr'
alias gpushcurr='git push origin `git curbranch`'
alias gpushcurrresolved='git push dc-master `git curbranch`:resolved'
alias gpo='git push origin'
alias gfm='git fetch dc-master'
alias codeReview='open "https://github.com/FishkinsDC/donorschoose-web/compare/DonorsChoose:resolved...`git curbranch`?w=1"'
alias syncMusic='rsync -r --delete /Users/fishkins/Music/iTunes/iTunes\ Media/Music/ /Volumes/FISHKINS/Music'
HISTFILE=~/.zhistory
HISTSIZE=SAVEHIST=10000
setopt incappendhistory 
setopt sharehistory
setopt extendedhistory
setopt extendedglob
unsetopt caseglob
EDITOR=emacs

autoload -U history-search-end

zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end

source .zshrcpersonal

# Improve git file path completion speed
__git_files () { 
    _wanted files expl 'local files' _files     
}

# Automatically quote globs in URL and remote references
__remote_commands=(scp rsync)
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
zstyle -e :urlglobber url-other-schema '[[ $__remote_commands[(i)$words[1]] -le ${#__remote_commands} ]] && reply=("*") || reply=(http https ftp)'

export PATH=/usr/local/bin:$PATH:$HOME/.rvm/bin:/usr/local/sbin:/usr/local/opt/ruby/bin # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" 

cd . # This triggers the function that sets pwd as the terminal header
