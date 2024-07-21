# cleo
alias cl="cd ~/work/cleo/meetcleo"
alias cl-ios="cl && cd packages/native-app && yarn && cd ios && bundle && be pod install --no-repo-update && yarn ios --simulator=\"iPhone 15 Pro (17.2)\""

# git
alias gadd="git add -N . && git add -p ."
alias grim="git fetch && git rebase -i --rebase-merges origin/main"

# prime translatrix
alias pt="cd ~/work/primetranslatrix/primetranslatrix-3"

# rails
alias be="bundle exec"
alias rdbm="bundle exec rails db:migrate"
alias rs="bundle exec rails server"
alias rc="bundle exec rails console"

# rusty bucket
alias rb="cd ~/work/rusty-bucket/rusty-bucket"

export EDITOR=vim
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
export NO_WEBPACK_COMPILE_CHECK=1
export SKIP_TEST_TIMEOUT=1
export LOG_PRODUCT_FEATURES_TO_DB=false
export SAVEHIST=1000000
export HISTFILE=~/.zsh_history
setopt APPEND_HISTORY
bindkey "^[^[[D" backward-word
bindkey "^[^[[C" forward-word
export ZSH="~/.oh-my-zsh"

ZSH_THEME="agnoster"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#808080"

plugins=(git extract sudo zsh-autosuggestions zsh-z)

source $ZSH/oh-my-zsh.sh

prompt_context(){}
