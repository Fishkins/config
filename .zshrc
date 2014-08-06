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
newcase() {
    branch_name=`echo $1 | sed "s/[ :\']\{1,\}/_/g"`
    git newcase $branch_name
}
gitdiffclass() {
    git wdiff src/**/$1.java
}
gitlogclass() {
    git logp src/**/$1.java
}
gitrebaseresolved() {
    git pull --rebase dc-master
}
gitselectbranch() {
    branches=`git branch | grep -v "^*" | grep -i $1`
    numBranches=`echo $branches | wc -w | awk '{print $1}'`

    while [ ! $numBranches -eq 1 ]
    do
	if [ $numBranches -eq 0 ]
	then
	    echo "No such branch. Choose one of these branches."
	    branches=`git branch | grep -v "^*"`
	else
	    echo "Multiple matches. Add another filter."
	fi

	echo $branches
	read newFilter
	branches=`echo $branches | grep -i $newFilter`
	numBranches=`echo $branches | wc -w | awk '{print $1}'`
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
codeReviewCommits() {
    open $(git log --pretty=oneline dc-master/resolved.. | awk '{print $1}' | sed -E 's_(.+)_https://github.com/FishkinsDC/donorschoose-web/commit/\1_' | tee >(pbcopy))
}
gpushcurrresolved() {
    git push dc-master `git curbranch`:resolved
}
alias gpushcurr='git push origin `git curbranch`'
alias psgrep='ps -ef | head -1;ps -ef | grep -v grep | egrep -i'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias ltr='ls -ltr'
alias gdeletemerged='git branch --merged=dc-master/resolved | grep BUGZID | grep -v "^*" | xargs git branch -d; git fetch origin --prune;'
alias gpo='git push origin'
alias gfm='git fetch dc-master'
alias codeReview='open $(echo "https://github.com/FishkinsDC/donorschoose-web/compare/DonorsChoose:resolved...$(git curbranch)?expand=1&w=1" | tee >(pbcopy))'
alias syncMusic='rsync -r --delete /Users/fishkins/Music/iTunes/iTunes\ Media/Music/ /Volumes/FISHKINS/Music'
alias reloadConfig='pushd ~; source .zshrc; popd;'
alias killMicrosoftDaemons="while true; do kill $(ps -ef | grep -v grep | egrep -i "(syncservicesag|database)" | awk '{print $2}'); done"
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
