# futurelearn
alias fl="cd /Users/jake/work/futurelearn/futurelearn"
alias kapow="fl && gup && fligo app refresh && be rails db:migrate && code ."
alias fljs="fl && fligo app start"
alias flrs="fl && be rails s"

function vpn {
  echo 'Application("Tunnelblick").connect("futurelearn")' | osascript -l JavaScript
}
function vpn-off {
  echo 'Application("Tunnelblick").disconnect("futurelearn")' | osascript -l JavaScript
}

# git
alias gadd="git add -N . && git add -p ."
alias grim="git fetch && git rebase -i origin/master"
alias grc="git rebase --continue"
alias gra="git rebase --abort"
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

# rusty bucket
alias rb="cd /Users/jake/work/rusty-bucket/rails"

# vscode
alias own-code="sudo chown -R $(whoami) /Applications/Visual\ Studio\ Code.app"
