alias wks="cd ~/workspace"

# eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)
# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/home/nana/.opam/opam-init/init.fish' && source '/home/nana/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true
# END opam configuration

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH

alias gs="git status"
alias gc="git commit"
alias gcm="git commit -m"
alias vim="nvim"
alias wks="cd ~/workspace"
alias work="wks && cd work"
alias gra="git remote add origin"

starship init fish | source

# pnpm
set -gx PNPM_HOME "/home/nana/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end
