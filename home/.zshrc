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

# put zsh at the top so that any clashing aliases we create take priority
export ZSH="$HOME/.oh-my-zsh"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#808080"

plugins=(git extract sudo zsh-autosuggestions z)

source $ZSH/oh-my-zsh.sh


# secrets
export AGENT_GITHUB_PAT="$(pass show agents/github)"
export AGENT_CIRCLECI_TOKEN="$(pass show agents/circleci)"
export HOMEBREW_GITHUB_API_TOKEN=$(pass show github/homebrew)


alias f=fzf
alias cat=bat
alias ls="eza --all --long --group"

# XDG
export XDG_CONFIG_HOME="$HOME/.config"
export CLAUDE_CONFIG_DIR="$XDG_CONFIG_HOME/claude"

# cleo
export AWS_PROFILE=Engineer-878877078763
export EAGER_LOAD_TEST_ENVIRONMENT=true
export LOG_PRODUCT_FEATURES_TO_DB=false
export NO_WEBPACK_COMPILE_CHECK=1
export PARALLEL_WORKERS=1
# export RAISE_API_EXCEPTIONS=true
export SKIP_TEST_TIMEOUT=1
export TERRAGRUNT_DOWNLOAD=${HOME}/.terragrunt-cache
export TERRAGRUNT_PROVIDER_CACHE=1
export TERRAGRUNT_PROVIDER_CACHE_DIR="$HOME/.terragrunt-provider-cache"
export TF_PLUGIN_CACHE_DIR=${HOME}/.terraform.d/plugin-cache
alias dracarys="bundle exec rails db:reset data:quick_sync_prod db:migrate db:seed stripe:setup_dev"

# git
alias gadd="git add -N . && git add -p ."
alias gfrm="git fetch && git rebase -i --rebase-merges origin/main"
alias kapow="git fetch --prune && gpr && bundle && bundle exec rails db:migrate && gco -- ."

# rails
alias be="bundle exec"
alias r="bundle exec rails"
alias rdbm="bundle exec rails db:migrate"
alias rs="bundle exec rails server"
alias rc="bundle exec rails console"

# rusty bucket
alias rb="cd ~/work/personal/rusty-bucket"

export EDITOR=vim
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES
export SAVEHIST=1000000
export HISTFILE=~/.zsh_history
setopt APPEND_HISTORY
bindkey "^[^[[D" backward-word
bindkey "^[^[[C" forward-word


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
eval "$(mise activate zsh)"
