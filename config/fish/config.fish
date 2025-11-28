# ------------------------------------------------------------------------------
# Environment Variables
# ------------------------------------------------------------------------------

# Bun
set -gx BUN_INSTALL "$HOME/.bun"
fish_add_path $BUN_INSTALL/bin

# pnpm
set -gx PNPM_HOME "/home/nana/.local/share/pnpm"
fish_add_path $PNPM_HOME

# Brew
eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)

# Opam
test -r '/home/nana/.opam/opam-init/init.fish' && source '/home/nana/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true

# ------------------------------------------------------------------------------
# Aliases
# ------------------------------------------------------------------------------

alias wks="cd ~/workspace"
alias work="wks && cd work"
alias gs="git status"
alias gc="git commit"
alias gcm="git commit -m"
alias gra="git remote add origin"
alias vim="nvim"

# ------------------------------------------------------------------------------
# Plugin Management (Fisher)
# ------------------------------------------------------------------------------

# Install fisher if not already installed
if not functions -q fisher
  curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher
end

# Plugins
fisher install jorgebucaran/nvm.fish
fisher install jethrokuan/z
fisher install PatrickF1/fzf.fish
fisher install fish-shell/fish-syntax-highlighting

# ------------------------------------------------------------------------------
# Starship Prompt
# ------------------------------------------------------------------------------

starship init fish | source
