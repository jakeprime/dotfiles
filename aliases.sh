alias kapow="cl && code ."

# cleo
alias cl="cd /Users/jake/work/cleo/meetcleo"
alias cl-ios="cl && cd packages/native-app && yarn && cd ios && bundle && be pod install --no-repo-update && yarn ios --simulator=\"iPhone 15 Pro (17.2)\""

# git
alias gadd="git add -N . && git add -p ."
alias grim="git fetch && git rebase -i --rebase-merges origin/main"
alias grc="git rebase --continue"
alias gra="git rebase --abort"
alias grimx="git fetch && git rebase --keep-empty -i origin HEAD:[branch_name] -x \"git push --force-with-lease\""
gqrb() {
  git set-upstream
  git fetch origin "$@:$@" && git pull && git rebase "$@"
}

gbir() {
  git rebase -i --autosquash $(git merge-base --fork-point "$@" $(git rev-parse --abbrev-ref HEAD))
}

# prime translatrix
alias pt="cd /Users/jake/work/primetranslatrix/primetranslatrix-3"

# rails
alias be="bundle exec"
alias rdbm="bundle exec rails db:migrate"
alias spring="bin/spring"
alias rs="bundle exec rails server"
alias rc="bundle exec rails console"

# rusty bucket
alias rb="cd /Users/jake/work/rusty-bucket/rusty-bucket"

# vscode
alias own-code="sudo chown -R $(whoami) /Applications/Visual\ Studio\ Code.app"

# OS
alias dockl="defaults write com.apple.dock orientation left && killall Dock"
alias dockb="defaults write com.apple.dock orientation bottom && killall Dock"
alias dockr="defaults write com.apple.dock orientation right && killall Dock"

alias xcode14="open /Applications/Xcode-14.app/Contents/MacOS/Xcode"
