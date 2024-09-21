# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt autocd
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jake/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

. ~/.asdf/plugins/java/set-java-home.zsh

alias H=Hyprland

# cleo
alias cl="cd ~/work/cleo/meetcleo"
alias clm="cd ~/work/cleo/mobile-app"
alias cl-ios="cl && cd packages/native-app && yarn && cd ios && bundle && be pod install --no-repo-update && yarn ios --simulator=\"iPhone 15 Pro (17.2)\""

export HOMEBREW_GITHUB_API_TOKEN=$(pass show github/homebrew)
export AWS_PROFILE=Engineer-878877078763

# git
alias gadd="git add -N . && git add -p ."
alias gfrm="git fetch && git rebase -i --rebase-merges origin/main"

# prime translatrix
alias pt="cd ~/work/primetranslatrix/primetranslatrix-3"

# rails
alias be="bundle exec"
alias rdbm="bundle exec rails db:migrate"
alias rs="bundle exec rails server"
alias rc="bundle exec rails console"

# rusty bucket
alias rb="cd ~/work/personal/rusty-bucket"

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
export ZSH="$HOME/.oh-my-zsh"
export LC_TIME="en_GB.UTF-8"

ZSH_THEME="agnoster"
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#808080"

plugins=(git extract sudo zsh-autosuggestions z)

source $ZSH/oh-my-zsh.sh

prompt_context(){}

source ~/.zshrc.local
