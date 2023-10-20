# -*- sh -*-
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
gpushcurrresolved() {
    echo "Are you sure you want to push to resolved? (y/n)"
    read confirmation

    if [[ $confirmation == y ]]; then
        git push dc $(git curbranch):resolved
    else
        echo "not pushing"
    fi
}
gitdiffclass() {
    git wdiff src/**/$1.java
}
gitlogclass() {
    git logp src/**/$1.java
}
copycase() {
    local curBranch=$(git curbranch)
    local newBranchName="${curBranch}_revised"
    git checkout -b $newBranchName $curBranch
    git branch $newBranchName -u dc/resolved
}
setupstream() {
    local repo="$(basename $(git rev-parse --show-toplevel))"
    local upstream=origin/master

    if [[ $repo == aws-devops ]]
    then
        upstream=origin/resolved
    elif [[ $repo == donorschoose-web ]]
    then
        upstream=dc/resolved
    fi

    git branch --set-upstream-to=$upstream
    git pull --rebase
}
casecommit() {
    local case=$(git curbranch | awk -F- '{print $1 "-" $2}')
    git ci -a -m "$case $*"
}
casecommit() {
    if [[ $(git curbranch) =~ ^(WS|IOPS) ]]
    then
        local CASE_ID="$(git curbranch | awk -F'-' '{print $1 "-" $2}') "
    fi

    git commit -a -m "${CASE_ID}$*"
}
alias gpushcurr='git push origin $(git curbranch)'
alias psgrep='ps -ef | head -1;ps -ef | grep -v grep | grep -E -i'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias ltr='ls -ltr'
alias gpo='git push origin'
alias reloadConfig='pushd ~; source .zshrc; popd;'
alias killFswatch="ps -ef | grep fswatch | grep -v grep | awk '{print $2}' | xargs kill"
alias noelcopy="tr -d '\n' | pbcopy"
alias en="emacsclient -n"
alias cdg="cd $DEV_SRC"
alias cdw="cd $DC_WEB"
alias cda="cd $AWS_DEVOPS_DIR"
alias cdf="cd $FASTLY_DIR"

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

# [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
source <(kubectl completion zsh)

cd . # This triggers the function that sets pwd as the terminal header

autoload -U +X bashcompinit && bashcompinit

. /opt/homebrew/opt/asdf/libexec/asdf.sh

deleteapplicationbranchesfromdockerrepos() {
    ls | grep -E "docker|fastly" | while read repo
    do
        pushd $repo
        git branch| grep 'WS-' | while read branch
        do
            git branch -D $branch
        done
        popd
    done
}

deleteUntrackedFiles() {
    git stat | grep "??" | cut -d' ' -f2
    echo "Are you sure you want delete the above files? (y/n)"
    read confirmation

    if [[ $confirmation == y ]]; then
        git stat | grep "??" | cut -d' ' -f2 | xargs rm
    else
        echo "not deleting"
    fi
}

# eks node management functions
function nodes() {
    local WORKPLANE="$1"
    if [ -z "$WORKPLANE" ]; then echo "ERROR: Specify workplane"; return 1; fi
    aws autoscaling describe-auto-scaling-groups --output=json \
        | jq -r "[.AutoScalingGroups
             |.[]
             |select(.AutoScalingGroupName|test(\".*-$WORKPLANE-nodes-.*\"))
             ] | map({(.AutoScalingGroupName): {
                      CreatedTime: .CreatedTime,
                      Instances: [.Instances[]]
             }})"
}

function ready() {
    kubectl get nodes -o json | \
        jq -r '.items
      |sort_by(.metadata.creationTimestamp)
      |.[]
      |select(.metadata.labels.online == "true")
      |{nodeName: .metadata.name,
        externalID: .spec.externalID,
        creationTimestamp: .metadata.creationTimestamp,
        conditions: .status.conditions[],
        instanceType: .metadata.labels["node.kubernetes.io/instance-type"],
        taintEffect: .spec.taints[0].effect
      }
      |select(.conditions.type == "Ready")
      |{nodeName,
        externalID,
        creationTimestamp,
        instanceType,
        ready: .conditions.reason,
        taintEffect
      }'
}

function pods() {
    local NAMESPACE=$1
    kubectl get pods --namespace=$NAMESPACE -o json \
        | jq -r '.items[]
             |{hostname: .metadata.name,
               nodeName: .spec.nodeName,
               phase: .status.phase
              }' 
}
