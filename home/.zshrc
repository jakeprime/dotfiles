# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt autocd
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jake/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

. ~/.asdf/plugins/java/set-java-home.zsh

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

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#808080"

plugins=(git extract sudo zsh-autosuggestions z)

source $ZSH/oh-my-zsh.sh


source ~/.zshrc.theme

prompt_context(){}

bindkey -v
# Set block cursor in normal mode
function zle-line-init {
  if [[ $KEYMAP == vicmd ]]; then
    echo -ne '\e[2 q'  # Block cursor (normal mode)
  else
    echo -ne '\e[3 q'  # Underline cursor (insert mode)
  fi
  zle reset-prompt
}

# Reset the cursor shape in insert mode
function zle-keymap-select {
  if [[ $KEYMAP == vicmd ]]; then
    echo -ne '\e[2 q'  # Block cursor (normal mode)
  else
    echo -ne '\e[3 q'  # Underline cursor (insert mode)
  fi
}

zle -N zle-line-init
zle -N zle-keymap-select

# create an alias so that we start Vim with a block cursor
alias vim="echo -ne \"\e[2 q\" && /usr/bin/vim"


source ~/.zshrc.local
