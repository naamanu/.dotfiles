#!/bin/bash
#
# macOS Software Engineering Tools Installation Script
# This script installs common development tools and utilities using Homebrew
#
############################

set -e  # Exit on error

echo "========================================="
echo "macOS Development Tools Installation"
echo "========================================="
echo ""

# Check if Homebrew is installed
if ! command -v brew &> /dev/null; then
    echo "Homebrew not found. Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Add Homebrew to PATH for Apple Silicon Macs
    if [[ $(uname -m) == 'arm64' ]]; then
        echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
else
    echo "Homebrew already installed. Updating..."
    brew update
fi

echo ""
echo "Installing core development tools..."
brew install git
brew install curl
brew install wget
brew install make
brew install cmake
brew install gcc

echo ""
echo "Installing shell and terminal tools..."
brew install fish              # Modern shell
brew install tmux              # Terminal multiplexer
brew install starship          # Cross-shell prompt
brew install zoxide            # Smart directory jumper
brew install direnv            # Environment variable manager

echo ""
echo "Installing modern CLI utilities..."
brew install neovim            # Modern vim
brew install ripgrep           # Fast grep alternative (rg)
brew install fd                # Fast find alternative
brew install fzf               # Fuzzy finder
brew install bat               # Cat with syntax highlighting
brew install eza               # Modern ls replacement
brew install jq                # JSON processor
brew install yq                # YAML processor
brew install htop              # Process viewer
brew install btop              # Modern htop alternative
brew install tree              # Directory tree viewer
brew install tldr              # Simplified man pages
brew install diff-so-fancy     # Better git diffs

echo ""
echo "Installing version control tools..."
brew install gh                # GitHub CLI
brew install lazygit           # Terminal UI for git
brew install git-delta         # Better diff viewer

echo ""
echo "Installing programming languages and runtimes..."
brew install node              # Node.js
brew install python@3.12       # Python 3
brew install go                # Go
brew install rust              # Rust
brew install lua               # Lua

echo ""
echo "Installing language servers and formatters..."
brew install lua-language-server
brew install stylua            # Lua formatter
brew install black             # Python formatter
brew install ruff              # Python linter
brew install prettier          # JS/TS formatter
brew install shfmt             # Shell script formatter
brew install shellcheck        # Shell script linter

echo ""
echo "Installing database tools..."
brew install postgresql@16
brew install sqlite
brew install redis

echo ""
echo "Installing Docker and container tools..."
brew install --cask docker
brew install lazydocker        # Docker TUI

echo ""
echo "Installing cloud and DevOps tools..."
brew install awscli            # AWS CLI
brew install terraform         # Infrastructure as code
brew install kubectl           # Kubernetes CLI
brew install k9s               # Kubernetes TUI

echo ""
echo "Installing productivity tools..."
brew install --cask rectangle  # Window management
brew install --cask ghostty    # Modern GPU-accelerated terminal
brew install --cask raycast    # Spotlight replacement

echo ""
echo "Installing fonts..."
# brew tap homebrew/cask-fonts
brew install --cask font-jetbrains-mono-nerd-font
brew install --cask font-fira-code-nerd-font
brew install --cask font-hack-nerd-font
brew install --cask font-inconsolata-nerd-font

echo ""
echo "Setting up fzf key bindings..."
$(brew --prefix)/opt/fzf/install --key-bindings --completion --no-update-rc

echo ""
echo "Setting up fish shell configuration..."
# Create fish config directory if it doesn't exist
mkdir -p ~/.config/fish

# Initialize starship for fish
if command -v starship &> /dev/null; then
    echo "starship init fish | source" >> ~/.config/fish/config.fish
fi

# Initialize zoxide for fish
if command -v zoxide &> /dev/null; then
    echo "zoxide init fish | source" >> ~/.config/fish/config.fish
fi

# Set up fish aliases and functions
cat >> ~/.config/fish/config.fish << 'EOF'

# Modern CLI tool aliases
if command -v eza &> /dev/null
    alias ls='eza --icons'
    alias ll='eza -l --icons'
    alias la='eza -la --icons'
    alias lt='eza --tree --icons'
end

if command -v bat &> /dev/null
    alias cat='bat'
end

# Git aliases
alias g='git'
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git pull'
alias glog='git log --oneline --graph --decorate'

# Directory navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Useful shortcuts
alias c='clear'
alias h='history'
alias vim='nvim'
alias v='nvim'

EOF

echo ""
echo "Setting up ghostty terminal configuration..."
mkdir -p ~/.config/ghostty
cat > ~/.config/ghostty/config << 'EOF'
# Ghostty Terminal Configuration

# Font configuration
font-family = "Inconsolata Nerd Font"
font-size = 14
font-thicken = true

# Window configuration
window-padding-x = 8
window-padding-y = 8
window-decoration = true
window-theme = dark

# Theme and colors
theme = dark
background-opacity = 0.95
unfocused-split-opacity = 0.7

# Shell
shell-integration = fish
shell-integration-features = cursor,sudo,title

# Keybindings
keybind = ctrl+shift+c=copy_to_clipboard
keybind = ctrl+shift+v=paste_from_clipboard
keybind = ctrl+shift+t=new_tab
keybind = ctrl+shift+w=close_surface
keybind = ctrl+tab=next_tab
keybind = ctrl+shift+tab=previous_tab

# Performance
copy-on-select = true

# Cursor
cursor-style = block
cursor-style-blink = true

EOF

echo ""
echo "========================================="
echo "Installation complete!"
echo "========================================="
echo ""
echo "Installed tools:"
echo "  - Core: git, curl, wget, make, cmake, gcc"
echo "  - Shell: fish, tmux, starship, zoxide"
echo "  - CLI: neovim, ripgrep, fd, fzf, bat, eza, jq, yq"
echo "  - Languages: node, python, go, rust, lua"
echo "  - LSP/Formatters: Various language servers and formatters"
echo "  - Git: gh, lazygit, git-delta"
echo "  - Databases: postgresql, sqlite, redis"
echo "  - Containers: docker, lazydocker"
echo "  - Cloud: awscli, terraform, kubectl, k9s"
echo ""
echo "Next steps:"
echo "  1. Restart your terminal or run: source ~/.zprofile"
echo "  2. Install Neovim plugins: nvim +Lazy sync +qa"
echo "  3. Set fish as default shell: chsh -s /opt/homebrew/bin/fish"
echo "  4. Configure git: git config --global user.name 'Your Name'"
echo "  5. Configure git: git config --global user.email 'your.email@example.com'"
echo ""
