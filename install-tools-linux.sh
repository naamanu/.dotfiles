#!/bin/bash
#
# Linux Software Engineering Tools Installation Script
# This script installs common development tools and utilities
# Supports: Ubuntu/Debian (apt), Fedora (dnf), Arch (pacman)
#
############################

set -e  # Exit on error

echo "========================================="
echo "Linux Development Tools Installation"
echo "========================================="
echo ""

# Detect package manager
if command -v apt &> /dev/null; then
    PKG_MGR="apt"
    PKG_UPDATE="sudo apt update"
    PKG_INSTALL="sudo apt install -y"
    echo "Detected package manager: apt (Debian/Ubuntu)"
elif command -v dnf &> /dev/null; then
    PKG_MGR="dnf"
    PKG_UPDATE="sudo dnf check-update || true"
    PKG_INSTALL="sudo dnf install -y"
    echo "Detected package manager: dnf (Fedora)"
elif command -v pacman &> /dev/null; then
    PKG_MGR="pacman"
    PKG_UPDATE="sudo pacman -Sy"
    PKG_INSTALL="sudo pacman -S --noconfirm"
    echo "Detected package manager: pacman (Arch)"
else
    echo "Error: No supported package manager found (apt, dnf, or pacman)"
    exit 1
fi

echo ""
echo "Updating package lists..."
$PKG_UPDATE

echo ""
echo "Installing core development tools..."
if [ "$PKG_MGR" = "apt" ]; then
    $PKG_INSTALL build-essential git curl wget cmake pkg-config libssl-dev
elif [ "$PKG_MGR" = "dnf" ]; then
    $PKG_INSTALL gcc gcc-c++ make git curl wget cmake openssl-devel
elif [ "$PKG_MGR" = "pacman" ]; then
    $PKG_INSTALL base-devel git curl wget cmake openssl
fi

echo ""
echo "Installing shell and terminal tools..."
$PKG_INSTALL fish tmux

# Install starship (cross-platform prompt)
if ! command -v starship &> /dev/null; then
    echo "Installing starship..."
    curl -sS https://starship.rs/install.sh | sh -s -- -y
fi

echo ""
echo "Installing modern CLI utilities..."
if [ "$PKG_MGR" = "apt" ]; then
    $PKG_INSTALL neovim ripgrep fd-find fzf bat htop tree jq
    # Create bat alias on Ubuntu (it's installed as batcat)
    mkdir -p ~/.local/bin
    ln -sf /usr/bin/batcat ~/.local/bin/bat 2>/dev/null || true
    # Create fd alias on Ubuntu (it's installed as fdfind)
    ln -sf /usr/bin/fdfind ~/.local/bin/fd 2>/dev/null || true
elif [ "$PKG_MGR" = "dnf" ]; then
    $PKG_INSTALL neovim ripgrep fd-find fzf bat htop tree jq
elif [ "$PKG_MGR" = "pacman" ]; then
    $PKG_INSTALL neovim ripgrep fd fzf bat htop tree jq
fi

# Install eza (modern ls replacement)
if ! command -v eza &> /dev/null; then
    echo "Installing eza..."
    if [ "$PKG_MGR" = "apt" ]; then
        wget -qO- https://raw.githubusercontent.com/eza-community/eza/main/deb.asc | sudo gpg --dearmor -o /etc/apt/keyrings/gierens.gpg
        echo "deb [signed-by=/etc/apt/keyrings/gierens.gpg] http://deb.gierens.de stable main" | sudo tee /etc/apt/sources.list.d/gierens.list
        sudo chmod 644 /etc/apt/keyrings/gierens.gpg /etc/apt/sources.list.d/gierens.list
        sudo apt update
        sudo apt install -y eza
    elif [ "$PKG_MGR" = "dnf" ]; then
        sudo dnf install -y eza
    elif [ "$PKG_MGR" = "pacman" ]; then
        sudo pacman -S --noconfirm eza
    fi
fi

# Install btop (modern htop alternative)
if ! command -v btop &> /dev/null; then
    echo "Installing btop..."
    if [ "$PKG_MGR" = "apt" ]; then
        $PKG_INSTALL btop
    elif [ "$PKG_MGR" = "dnf" ]; then
        $PKG_INSTALL btop
    elif [ "$PKG_MGR" = "pacman" ]; then
        $PKG_INSTALL btop
    fi
fi

# Install zoxide (smart directory jumper)
if ! command -v zoxide &> /dev/null; then
    echo "Installing zoxide..."
    curl -sS https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | bash
fi

echo ""
echo "Installing version control tools..."
if [ "$PKG_MGR" = "apt" ]; then
    # Install gh (GitHub CLI)
    if ! command -v gh &> /dev/null; then
        curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
        echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
        sudo apt update
        sudo apt install -y gh
    fi
elif [ "$PKG_MGR" = "dnf" ]; then
    $PKG_INSTALL gh
elif [ "$PKG_MGR" = "pacman" ]; then
    $PKG_INSTALL github-cli
fi

# Install lazygit
if ! command -v lazygit &> /dev/null; then
    echo "Installing lazygit..."
    LAZYGIT_VERSION=$(curl -s "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" | grep -Po '"tag_name": "v\K[^"]*')
    curl -Lo lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/latest/download/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
    tar xf lazygit.tar.gz lazygit
    sudo install lazygit /usr/local/bin
    rm lazygit lazygit.tar.gz
fi

# Install git-delta
if ! command -v delta &> /dev/null; then
    echo "Installing git-delta..."
    if [ "$PKG_MGR" = "apt" ]; then
        DELTA_VERSION=$(curl -s "https://api.github.com/repos/dandavison/delta/releases/latest" | grep -Po '"tag_name": "\K[^"]*')
        curl -Lo delta.deb "https://github.com/dandavison/delta/releases/latest/download/git-delta_${DELTA_VERSION}_amd64.deb"
        sudo dpkg -i delta.deb
        rm delta.deb
    elif [ "$PKG_MGR" = "dnf" ]; then
        $PKG_INSTALL git-delta
    elif [ "$PKG_MGR" = "pacman" ]; then
        $PKG_INSTALL git-delta
    fi
fi

echo ""
echo "Installing programming languages and runtimes..."

# Node.js (using nvm)
if ! command -v node &> /dev/null; then
    echo "Installing Node.js via nvm..."
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    nvm install --lts
fi

# Python
if [ "$PKG_MGR" = "apt" ]; then
    $PKG_INSTALL python3 python3-pip python3-venv
elif [ "$PKG_MGR" = "dnf" ]; then
    $PKG_INSTALL python3 python3-pip
elif [ "$PKG_MGR" = "pacman" ]; then
    $PKG_INSTALL python python-pip
fi

# Rust
if ! command -v rustc &> /dev/null; then
    echo "Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "$HOME/.cargo/env"
fi

# Go
if ! command -v go &> /dev/null; then
    echo "Installing Go..."
    GO_VERSION="1.21.5"
    wget "https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz"
    sudo rm -rf /usr/local/go
    sudo tar -C /usr/local -xzf "go${GO_VERSION}.linux-amd64.tar.gz"
    rm "go${GO_VERSION}.linux-amd64.tar.gz"
    echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.profile
fi

# Lua
$PKG_INSTALL lua5.4 || $PKG_INSTALL lua || true

echo ""
echo "Installing language servers and formatters..."

# Python tools
python3 -m pip install --user black ruff pyright

# Prettier for JS/TS
if command -v npm &> /dev/null; then
    npm install -g prettier eslint_d typescript-language-server
fi

# Lua tools
if command -v cargo &> /dev/null; then
    cargo install stylua
fi

# ShellCheck
$PKG_INSTALL shellcheck

echo ""
echo "Installing database tools..."
if [ "$PKG_MGR" = "apt" ]; then
    $PKG_INSTALL postgresql postgresql-contrib sqlite3
elif [ "$PKG_MGR" = "dnf" ]; then
    $PKG_INSTALL postgresql postgresql-server sqlite
elif [ "$PKG_MGR" = "pacman" ]; then
    $PKG_INSTALL postgresql sqlite
fi

echo ""
echo "Installing Docker..."
if ! command -v docker &> /dev/null; then
    if [ "$PKG_MGR" = "apt" ]; then
        # Docker official installation for Ubuntu/Debian
        curl -fsSL https://get.docker.com -o get-docker.sh
        sudo sh get-docker.sh
        sudo usermod -aG docker $USER
        rm get-docker.sh
    elif [ "$PKG_MGR" = "dnf" ]; then
        $PKG_INSTALL docker docker-compose
        sudo systemctl start docker
        sudo systemctl enable docker
        sudo usermod -aG docker $USER
    elif [ "$PKG_MGR" = "pacman" ]; then
        $PKG_INSTALL docker docker-compose
        sudo systemctl start docker
        sudo systemctl enable docker
        sudo usermod -aG docker $USER
    fi
fi

# Install lazydocker
if ! command -v lazydocker &> /dev/null; then
    echo "Installing lazydocker..."
    curl https://raw.githubusercontent.com/jesseduffield/lazydocker/master/scripts/install_update_linux.sh | bash
fi

echo ""
echo "Installing fonts..."
mkdir -p ~/.local/share/fonts

# JetBrains Mono Nerd Font
if [ ! -f ~/.local/share/fonts/JetBrainsMonoNerdFont-Regular.ttf ]; then
    echo "Installing JetBrains Mono Nerd Font..."
    wget https://github.com/ryanoasis/nerd-fonts/releases/download/v3.1.1/JetBrainsMono.zip
    unzip -o JetBrainsMono.zip -d ~/.local/share/fonts/
    rm JetBrainsMono.zip
fi

# Update font cache
fc-cache -fv

echo ""
echo "Setting up fzf key bindings..."
if [ -f /usr/share/doc/fzf/examples/key-bindings.bash ]; then
    echo "source /usr/share/doc/fzf/examples/key-bindings.bash" >> ~/.bashrc
fi

echo ""
echo "========================================="
echo "Installation complete!"
echo "========================================="
echo ""
echo "Installed tools:"
echo "  - Core: build tools, git, curl, wget, cmake"
echo "  - Shell: fish, tmux, starship, zoxide"
echo "  - CLI: neovim, ripgrep, fd, fzf, bat, eza, jq"
echo "  - Languages: node (via nvm), python3, rust, go, lua"
echo "  - LSP/Formatters: Various language servers and formatters"
echo "  - Git: gh, lazygit, git-delta"
echo "  - Databases: postgresql, sqlite"
echo "  - Containers: docker, lazydocker"
echo ""
echo "Next steps:"
echo "  1. Log out and back in for group changes to take effect (Docker)"
echo "  2. Restart your terminal or run: source ~/.bashrc"
echo "  3. Install Neovim plugins: nvim +Lazy sync +qa"
echo "  4. Set fish as default shell: chsh -s \$(which fish)"
echo "  5. Configure git: git config --global user.name 'Your Name'"
echo "  6. Configure git: git config --global user.email 'your.email@example.com'"
echo ""
