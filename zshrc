PROMPT='$ '
fpath=(/usr/local/share/zsh-completions ~/.zsh/completion $fpath)
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
gitselectbranch() {
    branches=$(git branch | grep -v "^*" | grep -i $1)
    numBranches=$(echo $branches | wc -w | awk '{print $1}')

    while [ ! $numBranches -eq 1 ]
    do
	if [ $numBranches -eq 0 ]
	then
	    echo "No such branch. Choose one of these branches."
	    branches=$(git branch | grep -v "^*")
	else
	    echo "Multiple matches. Add another filter."
	fi

	echo $branches
	read newFilter
	branches=$(echo $branches | grep -i $newFilter)
	numBranches=$(echo $branches | wc -w | awk '{print $1}')
    done
}
gitcheckout() {
    gitselectbranch $1
    echo $branches | xargs git checkout
}
copycase() {
    curBranch=$(git curbranch)
    newBranchName="${curBranch}_revised"
    git checkout -b $newBranchName $curBranch
    git branch $newBranchName -u dc-master/resolved
}
alias gpushcurr='git push origin $(git curbranch)'
alias psgrep='ps -ef | head -1;ps -ef | grep -v grep | egrep -i'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias ltr='ls -ltr'
alias gpo='git push origin'
alias reloadConfig='pushd ~; source .zshrc; popd;'
alias killFswatch="ps -ef | grep fswatch | grep -v grep | awk '{print $2}' | xargs kill"
alias noelcopy="tr -d '\n' | pbcopy"
alias en="emacsclient -n"
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

source ~/.zshrcpersonal

# Improve git file path completion speed
__git_files () {
    _wanted files expl 'local files' _files
}

# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1

# Set colors to match iTerm2 Terminal Colors
export TERM=xterm-256color

# Automatically quote globs in URL and remote references
__remote_commands=(scp rsync)
autoload -U url-quote-magic
zle -N self-insert url-quote-magic
zstyle -e :urlglobber url-other-schema '[[ $__remote_commands[(i)$words[1]] -le ${#__remote_commands} ]] && reply=("*") || reply=(http https ftp)'

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

cd . # This triggers the function that sets pwd as the terminal header

autoload -U +X bashcompinit && bashcompinit

. /opt/homebrew/opt/asdf/libexec/asdf.sh