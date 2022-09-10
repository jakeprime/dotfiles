# futurelearn
alias fl="cd /Users/jake/work/futurelearn/futurelearn"
alias kapow="ssh-add --apple-use-keychain ~/.ssh/futurelearn_rsa && fl && gup && brew services stop mysql@5.7 && fligo app services start && fligo app refresh && be rails db:migrate && code ."
alias fljs="fl && fligo app start"
alias flrs="fl && be rails s"

function vpn {
  echo 'Application("Tunnelblick").connect("futurelearn")' | osascript -l JavaScript
  notify
}
function vpn-off {
  echo 'Application("Tunnelblick").disconnect("futurelearn")' | osascript -l JavaScript
}

notify() {
  (sleep 1200 && osascript -e 'display notification "Is the VPN still on?"') &
}

# git
alias gadd="git add -N . && git add -p ."
alias grim="git fetch && git rebase -i origin/master"
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
alias rdbm="rails db:migrate"
alias spring="bin/spring"
alias rs="bundle exec rails server"
alias rc="bundle exec rails console"

# rusty bucket
alias rb="cd /Users/jake/work/rusty-bucket/rails"

# vscode
alias own-code="sudo chown -R $(whoami) /Applications/Visual\ Studio\ Code.app"
