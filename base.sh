source ~/.dotfiles/aliases.sh
source ~/.dotfiles/aws.sh
source ~/.dotfiles/env.sh
source ~/.dotfiles/goenv.sh
source ~/.dotfiles/key-bindings.sh
source ~/.dotfiles/nvm.sh
source ~/.dotfiles/python.sh
source ~/.dotfiles/ruby.sh
source ~/.dotfiles/scripts.sh
source ~/.dotfiles/secrets.sh
source ~/.dotfiles/zsh.sh

export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
export PATH="/Users/jake/.config/emacs/bin:$PATH"
export PATH="$PATH:/usr/local/bin/platform-tools/"

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export PATH="/opt/homebrew/opt/postgresql@12/bin:$PATH"
