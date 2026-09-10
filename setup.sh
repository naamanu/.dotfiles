#!/bin/bash
#
# Bootstrap script for setting up a new machine.
# Installs chezmoi, deploys dotfiles, installs development tools,
# and performs post-install setup.
#
# Usage:
#   bash <(curl -s https://raw.githubusercontent.com/naamanu/.dotfiles/main/setup.sh)
#
############################

FAILED_PACKAGES=()

# ── Optional components ───────────────────────────────────────
#
# Docker is heavyweight and not needed on every machine, so it is opt-in:
#
#   INSTALL_DOCKER=1 bash <(curl -s https://raw.githubusercontent.com/naamanu/.dotfiles/main/setup.sh)
#
INSTALL_DOCKER="${INSTALL_DOCKER:-0}"

CHECK_ONLY=0
case "${1:-}" in
    --check) CHECK_ONLY=1 ;;
    "") ;;
    *) echo "Usage: setup.sh [--check]" >&2; exit 2 ;;
esac

echo "========================================="
echo "  Dotfiles Bootstrap"
echo "========================================="
echo ""

# ── 1. Detect OS ──────────────────────────────────────────────

OS="$(uname -s)"
case "$OS" in
    Linux)  PLATFORM="linux" ;;
    Darwin) PLATFORM="mac" ;;
    *)
        echo "Error: Unsupported OS '$OS'"
        exit 1
        ;;
esac
echo "Detected platform: $PLATFORM"

# Architecture, spelled the way each upstream names its release assets.
case "$(uname -m)" in
    aarch64|arm64) ARCH=arm64;  ARCH_UNAME=aarch64; DEB_ARCH=arm64; NODE_ARCH=arm64 ;;
    *)             ARCH=x86_64; ARCH_UNAME=x86_64;  DEB_ARCH=amd64; NODE_ARCH=x64 ;;
esac

# Pinned versions, all in one place so staleness is visible. Everything else
# resolves "latest" at run time.
NERD_FONTS_VERSION="v3.1.1"
IOSEVKA_COMFY_VERSION="2.1.0"
LUA_LS_VERSION="${LUA_LS_VERSION:-3.18.2}"
BIBATA_VERSION="v2.0.7"
POSTGRES_FORMULA="postgresql@16"

# Ensure the install target is on PATH even if chezmoi is already present,
# so a chezmoi installed by a previous run is found in a fresh shell.
export PATH="$HOME/.local/bin:$PATH"

if [ "$CHECK_ONLY" -eq 1 ]; then
    if command -v dev-doctor &> /dev/null; then
        exec dev-doctor --all
    fi
    echo "dev-doctor is not installed yet; checking bootstrap prerequisites."
    missing=0
    for command in git curl bash; do
        if command -v "$command" &> /dev/null; then
            printf '[ok]      %s (%s)\n' "$command" "$(command -v "$command")"
        else
            printf '[MISSING] %s\n' "$command"
            missing=$((missing + 1))
        fi
    done
    exit "$missing"
fi

# ── 2. Install chezmoi ────────────────────────────────────────

if ! command -v chezmoi &> /dev/null; then
    echo ""
    echo "Installing chezmoi..."
    sh -c "$(curl -fsLS get.chezmoi.io)" -- -b "$HOME/.local/bin"
else
    echo "chezmoi already installed."
fi

if ! command -v chezmoi &> /dev/null; then
    echo "Error: chezmoi installation failed; cannot deploy dotfiles."
    exit 1
fi

# ── 3. Deploy dotfiles ────────────────────────────────────────

echo ""
echo "Initializing dotfiles with chezmoi..."

# Use the explicit repo URL. The `chezmoi init <username>` shorthand expands to
# <username>/dotfiles, which is a *different* repo than this one (.dotfiles).
DOTFILES_SSH_URL="git@github.com:naamanu/.dotfiles.git"
DOTFILES_HTTPS_URL="https://github.com/naamanu/.dotfiles.git"

# Prefer SSH, but only when a key is actually loaded and accepted by GitHub.
# On a brand-new machine the key usually does not exist yet, and an SSH clone
# fails before any dotfiles land. GitHub returns exit 1 on a successful auth
# ("does not provide shell access"), so match the greeting instead of the code.
if ssh -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10 \
       -o BatchMode=yes -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
    DOTFILES_URL="$DOTFILES_SSH_URL"
    echo "GitHub SSH auth OK; cloning over SSH."
else
    DOTFILES_URL="$DOTFILES_HTTPS_URL"
    echo "No working GitHub SSH key; cloning over HTTPS."
    echo "  (After adding a key, switch the remote with:"
    echo "   chezmoi git -- remote set-url origin $DOTFILES_SSH_URL)"
fi

SOURCE_DIR="$(chezmoi source-path 2>/dev/null || echo "$HOME/.local/share/chezmoi")"

DOTFILES_OK=1
if [ -d "$SOURCE_DIR/.git" ]; then
    # Already initialized: pull the latest and re-apply rather than re-cloning.
    echo "Existing chezmoi source dir found; updating in place."
    chezmoi update || DOTFILES_OK=0
else
    # Covers both "does not exist" and "exists but empty", the state left
    # behind by a previously failed clone.
    rmdir "$SOURCE_DIR" 2>/dev/null || true
    chezmoi init --apply "$DOTFILES_URL" || DOTFILES_OK=0
fi

if [ "$DOTFILES_OK" -ne 1 ]; then
    echo ""
    echo "Error: failed to deploy dotfiles from $DOTFILES_URL"
    echo "Fix the clone problem above and re-run this script; the tool"
    echo "installation below is skipped so the failure is not buried."
    exit 1
fi

echo "Dotfiles deployed."

# ── 4. Install development tools ─────────────────────────────

echo ""
echo "Installing development tools..."

if [ "$PLATFORM" = "mac" ]; then

    # ── macOS: Homebrew ───────────────────────────────────────

    if ! command -v brew &> /dev/null; then
        echo "Installing Homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        if [[ $(uname -m) == 'arm64' ]]; then
            grep -qxF 'eval "$(/opt/homebrew/bin/brew shellenv)"' ~/.zprofile 2>/dev/null || \
                echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
            eval "$(/opt/homebrew/bin/brew shellenv)"
        fi
    else
        echo "Homebrew already installed. Updating..."
        brew update
    fi

    # Helper: install brew packages, track failures, never exit
    brew_install() {
        for pkg in "$@"; do
            brew install "$pkg" || FAILED_PACKAGES+=("$pkg")
        done
    }

    brew_cask_install() {
        for pkg in "$@"; do
            brew install --cask "$pkg" || FAILED_PACKAGES+=("cask:$pkg")
        done
    }

    echo ""
    echo "Installing core development tools..."
    brew_install git curl wget make cmake gcc

    echo ""
    echo "Installing shell and terminal tools..."
    brew_install fish tmux starship zoxide direnv fnm
    # GNU coreutils (as g-prefixed binaries): Emacs Dired uses `gls' for
    # --group-directories-first, which BSD ls lacks.
    brew_install coreutils
    # emacs-libvterm compiles a native module against the system libvterm and
    # needs its headers, not just the runtime library.
    brew_install libvterm

    # The `emacs' formula is built without native compilation; the emacs-app
    # cask (emacsformacosx.com build) has it, so the config is much faster
    # with the cask.  Link its binaries into ~/.local/bin, which the fish
    # config puts ahead of /opt/homebrew/bin.
    if brew list --formula emacs &> /dev/null; then
        brew uninstall emacs || true
    fi
    brew_cask_install emacs-app
    if [ -x /Applications/Emacs.app/Contents/MacOS/Emacs ]; then
        mkdir -p "$HOME/.local/bin"
        ln -sf /Applications/Emacs.app/Contents/MacOS/Emacs "$HOME/.local/bin/emacs"
        ln -sf /Applications/Emacs.app/Contents/MacOS/bin/emacsclient "$HOME/.local/bin/emacsclient"
    fi

    echo ""
    echo "Installing modern CLI utilities..."
    brew_install neovim ripgrep fd fzf bat eza jq yq htop btop tldr
    # nvim-treesitter (main branch) compiles parsers via the tree-sitter CLI
    brew_install tree-sitter-cli
    brew_install yazi atuin glow dust procs hyperfine tokei

    echo ""
    echo "Installing version control tools..."
    brew_install gh lazygit git-delta

    echo ""
    echo "Installing programming languages and runtimes..."
    # brew's haskell-language-server is built against brew's ghc, so keep the
    # two in step (both from brew, upgraded together) rather than mixing in
    # ghcup.
    # Rust comes from rustup (shared post-install section) so rust-analyzer,
    # clippy and rustfmt are components of the same toolchain; brew's rust
    # would shadow it.
    brew_install go lua uv ghc cabal-install haskell-language-server ormolu

    echo ""
    echo "Installing C/C++, OCaml, FP and Lisp toolchains..."
    # clangd comes from the Xcode CLT; brew llvm is keg-only, so install
    # clang-format standalone to get it on PATH.  lldb-dap (for Emacs dape)
    # is linked out of the keg below.
    brew_install llvm bear clang-format
    brew_install ocaml opam dune
    # Racket (minimal distribution; `raco pkg install' adds libraries),
    # Standard ML (SML/NJ REPL + millet language server), Common Lisp.
    brew_install minimal-racket smlnj millet
    brew_install sbcl
    # Prefer the Xcode CLT copy: brew's llvm keg can be autoremoved, which
    # leaves a dangling link.
    LLDB_DAP="$(xcrun -f lldb-dap 2>/dev/null || true)"
    [ -x "$LLDB_DAP" ] || LLDB_DAP=/opt/homebrew/opt/llvm/bin/lldb-dap
    if [ -x "$LLDB_DAP" ]; then
        mkdir -p "$HOME/.local/bin"
        ln -sf "$LLDB_DAP" "$HOME/.local/bin/lldb-dap"
    fi

    # ocaml-lsp-server, ocamlformat and merlin are OPAM packages, not Homebrew
    # formulae — installing them with brew always failed silently.
    if command -v opam &> /dev/null; then
        if [ ! -d "$HOME/.opam" ]; then
            opam init -a --disable-sandboxing --shell-setup || true
        fi
        eval "$(opam env 2>/dev/null)" || true
        opam install -y ocaml-lsp-server ocamlformat merlin utop || true
    fi

    echo ""
    echo "Installing language servers and formatters..."
    brew_install lua-language-server stylua ruff shfmt shellcheck texlab
    # Emacs pdf-tools compiles its epdfinfo server against these on first use.
    brew_install poppler automake
    # Spellchecking backend for Emacs jinx.
    brew_install enchant

    echo ""
    echo "Installing database tools..."
    brew_install "$POSTGRES_FORMULA" sqlite redis

    echo ""
    if [ "$INSTALL_DOCKER" = "1" ]; then
        echo "Installing Docker and container tools..."
        brew_cask_install docker-desktop
        brew_install lazydocker
    else
        echo "Skipping Docker (set INSTALL_DOCKER=1 to install Docker Desktop + lazydocker)."
    fi

    echo ""
    echo "Installing cloud and DevOps tools..."
    brew_install awscli terraform kubectl k9s

    echo ""
    echo "Installing ML/AI & scientific tools..."
    # jupyterlab and ipython are uv tools (shared section), not brew formulae.
    brew_install pandoc typst ollama
    # Org LaTeX previews render through dvisvgm; core.el points it at the
    # texlive formula's TEXMF tree.
    brew_install dvisvgm
    brew_cask_install mactex-no-gui

    echo ""
    echo "Installing productivity tools..."
    brew_cask_install ghostty raycast
    brew_cask_install nikitabobko/tap/aerospace jordanbaird-ice

    echo ""
    echo "Installing fonts..."
    brew_cask_install font-jetbrains-mono-nerd-font font-fira-code-nerd-font font-hack-nerd-font font-inconsolata-nerd-font
    # Emacs offers these as fontaine presets (C-c t f); Iosevka Comfy is
    # Protesilaos's font, tuned for exactly this kind of setup.
    brew_cask_install font-iosevka-comfy font-commit-mono-nerd-font
    # Icon glyphs for nerd-icons/doom-modeline when the main font is not a
    # Nerd Font (core.el routes the private-use ranges to it).
    brew_cask_install font-symbols-only-nerd-font

    # fzf key bindings come from functions/fish_user_key_bindings.fish
    # (`fzf --fish`); the legacy install script would write an unmanaged copy.

elif [ "$PLATFORM" = "linux" ]; then

    # ── Linux: apt / dnf / pacman ─────────────────────────────

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

    install_pkg() {
        $PKG_INSTALL "$@" || FAILED_PACKAGES+=("$@")
    }

    install_optional_pkg() {
        local pkg="$1"
        $PKG_INSTALL "$pkg" > /dev/null 2>&1 || {
            echo "Skipping optional package: $pkg (not available)."
            return 0
        }
        echo "Installed optional package: $pkg"
    }

    echo ""
    echo "Updating package lists..."
    $PKG_UPDATE

    echo ""
    echo "Installing core development tools..."
    if [ "$PKG_MGR" = "apt" ]; then
        install_pkg build-essential git curl wget cmake pkg-config libssl-dev
    elif [ "$PKG_MGR" = "dnf" ]; then
        install_pkg gcc gcc-c++ make git curl wget cmake openssl-devel
    elif [ "$PKG_MGR" = "pacman" ]; then
        install_pkg base-devel git curl wget cmake openssl
    fi

    echo ""
    echo "Installing shell and terminal tools..."
    if [ "$PKG_MGR" = "apt" ]; then
        # Ubuntu ships fish 3.x (noble: 3.7.0) but the config targets fish 4:
        # atuin's init picks its `bind` syntax off $version, and fish 4.3
        # drops conf.d/fish_frozen_key_bindings.fish into the config. macOS
        # gets current fish from brew, so use upstream's PPA to match.
        # add-apt-repository is idempotent, and the PPA is Ubuntu-only, so
        # Debian keeps the distro package.
        if [ "$(. /etc/os-release 2>/dev/null && echo "$ID")" = "ubuntu" ]; then
            install_optional_pkg software-properties-common
            sudo add-apt-repository -y ppa:fish-shell/release-4 && $PKG_UPDATE
        fi
        install_pkg fish tmux
        # Debian/Ubuntu package Emacs 29, but the config targets Emacs 30+
        # (which-key and editorconfig are expected as built-ins). The
        # classic-confined snap tracks current releases; symlink it into
        # ~/.local/bin so it shadows any apt-installed /usr/bin/emacs.
        if command -v snap &> /dev/null; then
            sudo snap install emacs --classic || FAILED_PACKAGES+=(emacs)
            mkdir -p ~/.local/bin
            ln -sf /snap/bin/emacs ~/.local/bin/emacs
            if [ -e /snap/bin/emacsclient ]; then
                ln -sf /snap/bin/emacsclient ~/.local/bin/emacsclient
            else
                ln -sf /snap/bin/emacs.emacsclient ~/.local/bin/emacsclient
            fi
        else
            install_pkg emacs
        fi
    else
        install_pkg fish tmux emacs
    fi

    # tmux-yank needs a clipboard helper, or `y' in copy-mode quietly copies
    # to the tmux buffer only and never reaches the system clipboard. macOS
    # has pbcopy built in; Linux has nothing by default.
    #
    # tmux-yank probes wl-copy BEFORE xsel and does not look at the session
    # type, so wl-clipboard present on an X11 machine makes every yank fail.
    # Install it only where Wayland is actually in use.
    install_pkg xsel
    if [ "${XDG_SESSION_TYPE:-}" = "wayland" ] || [ -n "${WAYLAND_DISPLAY:-}" ]; then
        install_pkg wl-clipboard
    fi

    # emacs-libvterm compiles a native module against the system libvterm and
    # needs its headers, not just the runtime library. Without them the build
    # fails with LIBVTERM_INCLUDE_DIR-NOTFOUND and vterm reports at runtime
    # that libvterm does not exist.
    if [ "$PKG_MGR" = "apt" ]; then
        install_pkg libvterm-dev
    elif [ "$PKG_MGR" = "dnf" ]; then
        install_pkg libvterm-devel
    elif [ "$PKG_MGR" = "pacman" ]; then
        install_pkg libvterm
    fi

    if ! command -v starship &> /dev/null; then
        echo "Installing starship..."
        curl -sS https://starship.rs/install.sh | sh -s -- -y || FAILED_PACKAGES+=(starship)
    fi

    echo ""
    echo "Installing modern CLI utilities..."
    if [ "$PKG_MGR" = "apt" ]; then
        install_pkg ripgrep fd-find fzf bat htop jq
        mkdir -p ~/.local/bin
        ln -sf /usr/bin/batcat ~/.local/bin/bat 2>/dev/null || true
        ln -sf /usr/bin/fdfind ~/.local/bin/fd 2>/dev/null || true
        # Debian/Ubuntu package Neovim far behind stable (noble: 0.9.5), but
        # the nvim config needs >= 0.10 (vim.uv). Install the official
        # release tarball under ~/.local instead; ~/.local/bin shadows any
        # apt-installed /usr/bin/nvim.
        nvim_tmp="$(mktemp -d)"
        if curl -fsSL "https://github.com/neovim/neovim/releases/latest/download/nvim-linux-${ARCH}.tar.gz" -o "$nvim_tmp/nvim.tar.gz" \
            && tar xzf "$nvim_tmp/nvim.tar.gz" -C "$nvim_tmp"; then
            rm -rf ~/.local/opt/nvim
            mkdir -p ~/.local/opt
            mv "$nvim_tmp/nvim-linux-${ARCH}" ~/.local/opt/nvim
            ln -sf ~/.local/opt/nvim/bin/nvim ~/.local/bin/nvim
        else
            FAILED_PACKAGES+=(neovim)
        fi
        rm -rf "$nvim_tmp"
    elif [ "$PKG_MGR" = "dnf" ]; then
        install_pkg neovim ripgrep fd-find fzf bat htop jq
    elif [ "$PKG_MGR" = "pacman" ]; then
        install_pkg neovim ripgrep fd fzf bat htop jq
    fi

    # nvim-treesitter (main branch) compiles parsers via `tree-sitter build`,
    # which needs CLI >= 0.22. Distro packages are often missing or stale
    # (noble ships 0.20.8, which lacks the build subcommand), so a mere
    # `command -v` check is not enough — verify the version too.
    tree_sitter_ok() {
        command -v tree-sitter &> /dev/null || return 1
        local v
        v=$(tree-sitter --version | awk '{print $2}')
        [ "$(printf '%s\n' 0.22.0 "$v" | sort -V | head -1)" = "0.22.0" ]
    }
    if ! tree_sitter_ok; then
        ts_tmp="$(mktemp -d)"
        if curl -fsSL "https://github.com/tree-sitter/tree-sitter/releases/latest/download/tree-sitter-linux-${NODE_ARCH}.gz" -o "$ts_tmp/tree-sitter.gz" \
            && gunzip -f "$ts_tmp/tree-sitter.gz"; then
            mkdir -p ~/.local/bin
            install -m 755 "$ts_tmp/tree-sitter" ~/.local/bin/tree-sitter
            rm -rf "$ts_tmp"
        elif command -v cargo &> /dev/null; then
            cargo install tree-sitter-cli
        else
            FAILED_PACKAGES+=(tree-sitter-cli)
        fi
    fi

    if ! command -v eza &> /dev/null; then
        echo "Installing eza..."
        if [ "$PKG_MGR" = "apt" ]; then
            sudo install -m 0755 -d /etc/apt/keyrings
            wget -qO- https://raw.githubusercontent.com/eza-community/eza/main/deb.asc | sudo gpg --dearmor -o /etc/apt/keyrings/gierens.gpg
            echo "deb [signed-by=/etc/apt/keyrings/gierens.gpg] http://deb.gierens.de stable main" | sudo tee /etc/apt/sources.list.d/gierens.list
            sudo chmod 644 /etc/apt/keyrings/gierens.gpg /etc/apt/sources.list.d/gierens.list
            sudo apt update
            sudo apt install -y eza || FAILED_PACKAGES+=(eza)
        else
            install_pkg eza
        fi
    fi

    if ! command -v btop &> /dev/null; then
        install_pkg btop
    fi

    if ! command -v zoxide &> /dev/null; then
        echo "Installing zoxide..."
        curl -sS https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | bash || FAILED_PACKAGES+=(zoxide)
    fi

    echo ""
    echo "Installing additional modern CLI tools..."
    install_optional_pkg dust
    install_optional_pkg procs
    install_optional_pkg hyperfine
    install_optional_pkg tokei
    install_optional_pkg glow

    if ! command -v yazi &> /dev/null; then
        if command -v cargo &> /dev/null; then
            cargo install yazi-fm yazi-cli || FAILED_PACKAGES+=(yazi)
        fi
    fi

    if ! command -v atuin &> /dev/null; then
        curl --proto '=https' --tlsv1.2 -LsSf https://setup.atuin.sh | sh || FAILED_PACKAGES+=(atuin)
    fi

    echo ""
    echo "Installing version control tools..."
    if [ "$PKG_MGR" = "apt" ]; then
        if ! command -v gh &> /dev/null; then
            curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
            echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
            sudo apt update
            sudo apt install -y gh || FAILED_PACKAGES+=(gh)
        fi
    elif [ "$PKG_MGR" = "dnf" ]; then
        install_pkg gh
    elif [ "$PKG_MGR" = "pacman" ]; then
        install_pkg github-cli
    fi

    if ! command -v lazygit &> /dev/null; then
        echo "Installing lazygit..."
        LAZYGIT_VERSION=$(curl -s "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" | grep -Po '"tag_name": "v\K[^"]*')
        lazygit_tmp="$(mktemp -d)"
        if curl -fsSLo "$lazygit_tmp/lazygit.tar.gz" "https://github.com/jesseduffield/lazygit/releases/latest/download/lazygit_${LAZYGIT_VERSION}_Linux_${ARCH}.tar.gz" \
            && tar xf "$lazygit_tmp/lazygit.tar.gz" -C "$lazygit_tmp" lazygit; then
            install -m 755 "$lazygit_tmp/lazygit" ~/.local/bin/lazygit
        else
            FAILED_PACKAGES+=(lazygit)
        fi
        rm -rf "$lazygit_tmp"
    fi

    if ! command -v delta &> /dev/null; then
        echo "Installing git-delta..."
        if [ "$PKG_MGR" = "apt" ]; then
            DELTA_VERSION=$(curl -s "https://api.github.com/repos/dandavison/delta/releases/latest" | grep -Po '"tag_name": "\K[^"]*')
            delta_tmp="$(mktemp -d)"
            if curl -fsSLo "$delta_tmp/delta.deb" "https://github.com/dandavison/delta/releases/latest/download/git-delta_${DELTA_VERSION}_${DEB_ARCH}.deb"; then
                sudo dpkg -i "$delta_tmp/delta.deb" || FAILED_PACKAGES+=(git-delta)
            else
                FAILED_PACKAGES+=(git-delta)
            fi
            rm -rf "$delta_tmp"
        else
            install_pkg git-delta
        fi
    fi

    echo ""
    echo "Installing programming languages and runtimes..."

    # fnm is the one Node version manager (the fish config hooks it); Node
    # itself is installed in the shared post-install section.
    if ! command -v fnm &> /dev/null; then
        echo "Installing fnm (Node version manager)..."
        fnm_tmp="$(mktemp -d)"
        FNM_ASSET=fnm-linux.zip
        [ "$ARCH" = arm64 ] && FNM_ASSET=fnm-arm64.zip
        if curl -fsSLo "$fnm_tmp/fnm.zip" "https://github.com/Schniz/fnm/releases/latest/download/$FNM_ASSET" \
            && unzip -oq "$fnm_tmp/fnm.zip" -d "$fnm_tmp"; then
            mkdir -p ~/.local/bin
            install -m 755 "$fnm_tmp/fnm" ~/.local/bin/fnm
        else
            FAILED_PACKAGES+=(fnm)
        fi
        rm -rf "$fnm_tmp"
    fi

    if ! command -v uv &> /dev/null; then
        echo "Installing uv..."
        curl -LsSf https://astral.sh/uv/install.sh | sh || FAILED_PACKAGES+=(uv)
    fi

    if ! command -v rustc &> /dev/null; then
        echo "Installing Rust..."
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        source "$HOME/.cargo/env"
    fi

    if ! command -v go &> /dev/null; then
        echo "Installing Go..."
        GO_VERSION=$(curl -s 'https://go.dev/VERSION?m=text' | head -1 | sed 's/go//')
        go_tmp="$(mktemp -d)"
        if curl -fsSLo "$go_tmp/go.tar.gz" "https://go.dev/dl/go${GO_VERSION}.linux-${DEB_ARCH}.tar.gz"; then
            sudo rm -rf /usr/local/go
            sudo tar -C /usr/local -xzf "$go_tmp/go.tar.gz"
        else
            FAILED_PACKAGES+=(go)
        fi
        rm -rf "$go_tmp"
        echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.profile
    fi

    $PKG_INSTALL lua5.4 || $PKG_INSTALL lua || true

    echo ""
    echo "Installing C/C++, OCaml, and Lisp toolchains..."
    if [ "$PKG_MGR" = "apt" ]; then
        install_pkg clang clangd clang-format lldb bear ocaml opam
    elif [ "$PKG_MGR" = "dnf" ]; then
        # clang-tools-extra provides clangd and clang-format
        install_pkg clang clang-tools-extra lldb bear ocaml opam
    elif [ "$PKG_MGR" = "pacman" ]; then
        # clang provides clangd and clang-format
        install_pkg clang llvm lldb bear ocaml opam
    fi

    install_optional_pkg dune
    install_optional_pkg ocaml-dune
    install_optional_pkg sbcl
    # Racket and Standard ML for the Emacs racket-mode / sml-mode setup;
    # millet (SML LSP) is a cargo install below.  Enchant backs Emacs jinx.
    install_optional_pkg racket
    install_optional_pkg smlnj
    install_optional_pkg enchant-2
    install_optional_pkg libenchant-2-2

    if command -v opam &> /dev/null; then
        if [ ! -d "$HOME/.opam" ]; then
            opam init -a --disable-sandboxing --shell-setup || true
        fi
        eval "$(opam env 2>/dev/null)" || true
        opam install -y ocaml-lsp-server ocamlformat merlin utop || true
    fi

    echo ""
    echo "Installing language servers and formatters..."
    # npm servers, stylua, rustup components and the Go tools are installed
    # in the shared post-install section below; only the Linux-specific
    # pieces are here.
    if command -v cargo &> /dev/null && ! command -v millet &> /dev/null; then
        cargo install millet-ls || FAILED_PACKAGES+=(millet)
    fi

    install_pkg shellcheck
    install_optional_pkg shfmt

    # Emacs pdf-tools compiles its epdfinfo server against these on first use.
    if [ "$PKG_MGR" = "apt" ]; then
        install_optional_pkg libpoppler-glib-dev
        install_optional_pkg libpoppler-private-dev
        install_optional_pkg libpng-dev
        install_optional_pkg zlib1g-dev
        install_optional_pkg automake
    elif [ "$PKG_MGR" = "dnf" ]; then
        install_optional_pkg poppler-glib-devel
        install_optional_pkg libpng-devel
        install_optional_pkg automake
    elif [ "$PKG_MGR" = "pacman" ]; then
        install_optional_pkg poppler-glib
        install_optional_pkg libpng
        install_optional_pkg automake
    fi

    echo ""
    echo "Installing database tools..."
    if [ "$PKG_MGR" = "apt" ]; then
        install_pkg postgresql postgresql-contrib sqlite3
    elif [ "$PKG_MGR" = "dnf" ]; then
        install_pkg postgresql postgresql-server sqlite
    elif [ "$PKG_MGR" = "pacman" ]; then
        install_pkg postgresql sqlite
    fi

    echo ""
    if [ "$INSTALL_DOCKER" = "1" ]; then
        echo "Installing Docker..."
        if ! command -v docker &> /dev/null; then
            if [ "$PKG_MGR" = "apt" ]; then
                curl -fsSL https://get.docker.com -o get-docker.sh
                sudo sh get-docker.sh
                sudo usermod -aG docker "$USER"
                rm get-docker.sh
            else
                install_pkg docker docker-compose
                sudo systemctl start docker
                sudo systemctl enable docker
                sudo usermod -aG docker "$USER"
            fi
        fi

        if ! command -v lazydocker &> /dev/null; then
            echo "Installing lazydocker..."
            curl https://raw.githubusercontent.com/jesseduffield/lazydocker/master/scripts/install_update_linux.sh | bash || FAILED_PACKAGES+=(lazydocker)
        fi
    else
        echo "Skipping Docker (set INSTALL_DOCKER=1 to install Docker + lazydocker)."
    fi

    echo ""
    echo "Installing ML/AI & scientific tools..."
    install_pkg pandoc
    if [ "$PKG_MGR" = "apt" ]; then
        # texlive-full is several GB; this covers org/LaTeX previews and papers.
        install_pkg texlive-latex-extra texlive-fonts-recommended texlive-extra-utils texlive-bibtex-extra biber
    elif [ "$PKG_MGR" = "dnf" ]; then
        install_pkg texlive-scheme-full
    elif [ "$PKG_MGR" = "pacman" ]; then
        install_pkg texlive-most
    fi

    if ! command -v typst &> /dev/null; then
        if command -v cargo &> /dev/null; then
            cargo install typst-cli || FAILED_PACKAGES+=(typst)
        fi
    fi

    if ! command -v ollama &> /dev/null; then
        curl -fsSL https://ollama.com/install.sh | sh || FAILED_PACKAGES+=(ollama)
    fi

    echo ""
    echo "Installing Ghostty terminal..."
    if ! command -v ghostty &> /dev/null; then
        if [ "$PKG_MGR" = "apt" ]; then
            sudo apt install -y libgtk-4-dev libadwaita-1-dev git
            /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/mkasberg/ghostty-ubuntu/HEAD/install.sh)" || FAILED_PACKAGES+=(ghostty)
        elif [ "$PKG_MGR" = "pacman" ]; then
            echo "Note: Install ghostty from AUR using your AUR helper"
            echo "Example: yay -S ghostty"
        fi
    fi

    echo ""
    echo "Installing fonts..."
    mkdir -p ~/.local/share/fonts

    # $1 = release asset basename, $2 = installed-filename prefix used to
    # detect an existing install. Downloads into a temp dir so a failed or
    # interrupted run leaves no zip behind in the working directory.
    install_nerd_font() {
        if compgen -G "$HOME/.local/share/fonts/$2*" > /dev/null; then
            return
        fi
        echo "Installing $1 Nerd Font..."
        local tmp
        tmp="$(mktemp -d)" || { FAILED_PACKAGES+=("font:$1"); return; }
        if wget -q --show-progress -O "$tmp/$1.zip" \
            "https://github.com/ryanoasis/nerd-fonts/releases/download/$NERD_FONTS_VERSION/$1.zip" \
            && unzip -oq "$tmp/$1.zip" -d "$tmp/extract"; then
            # Font files only; the archives also carry LICENSE and README.md.
            find "$tmp/extract" \( -name '*.ttf' -o -name '*.otf' \) \
                -exec cp -f {} ~/.local/share/fonts/ \;
        else
            FAILED_PACKAGES+=("font:$1")
        fi
        rm -rf "$tmp"
    }

    install_nerd_font JetBrainsMono JetBrainsMonoNerdFont
    install_nerd_font FiraCode FiraCodeNerdFont
    install_nerd_font Hack HackNerdFont
    install_nerd_font Inconsolata InconsolataNerdFont
    # Emacs offers these as fontaine presets (C-c t f); Iosevka Comfy is
    # Protesilaos's font, tuned for exactly this kind of setup.
    install_nerd_font CommitMono CommitMonoNerdFont
    # Icon glyphs for nerd-icons/doom-modeline when the main font is not a
    # Nerd Font (core.el routes the private-use ranges to it).
    install_nerd_font NerdFontsSymbolsOnly SymbolsNerdFont

    # Iosevka Comfy publishes no release archive, so the Homebrew cask builds
    # from the tag tarball and installs every */TTF/*.ttf. Mirror that. It is
    # ~1 GB unpacked across 126 files, so it is skipped when already present.
    if ! compgen -G "$HOME/.local/share/fonts/iosevka-comfy*" > /dev/null; then
        echo "Installing Iosevka Comfy (large download, ~1 GB unpacked)..."
        iosevka_tmp="$(mktemp -d)"
        if wget -q --show-progress -O "$iosevka_tmp/iosevka-comfy.tar.gz" \
            "https://github.com/protesilaos/iosevka-comfy/archive/refs/tags/$IOSEVKA_COMFY_VERSION.tar.gz"; then
            if tar -xzf "$iosevka_tmp/iosevka-comfy.tar.gz" -C "$iosevka_tmp" \
                --wildcards '*/TTF/*.ttf'; then
                find "$iosevka_tmp" -name '*.ttf' -exec cp -f {} ~/.local/share/fonts/ \;
            else
                FAILED_PACKAGES+=("font:iosevka-comfy")
            fi
        else
            FAILED_PACKAGES+=("font:iosevka-comfy")
        fi
        rm -rf "$iosevka_tmp"
    fi

    fc-cache -f

    # ── Terminal tools without an apt package ─────────────────
    #
    # Neither is in the Ubuntu archive, and both publish a static release, so
    # take it directly rather than adding a PPA for two binaries.

    if ! command -v yazi &> /dev/null; then
        echo "Installing yazi (terminal file manager)..."
        yazi_tmp="$(mktemp -d)"
        if curl -fsSL -o "$yazi_tmp/yazi.zip" \
            "https://github.com/sxyazi/yazi/releases/latest/download/yazi-${ARCH_UNAME}-unknown-linux-gnu.zip" \
            && unzip -oq "$yazi_tmp/yazi.zip" -d "$yazi_tmp"; then
            find "$yazi_tmp" -type f \( -name yazi -o -name ya \) \
                -exec install -m 755 {} "$HOME/.local/bin/" \;
        else
            FAILED_PACKAGES+=(yazi)
        fi
        rm -rf "$yazi_tmp"
    fi

    if ! command -v fastfetch &> /dev/null; then
        echo "Installing fastfetch..."
        ff_tmp="$(mktemp -d)"
        if curl -fsSL -o "$ff_tmp/fastfetch.deb" \
            "https://github.com/fastfetch-cli/fastfetch/releases/latest/download/fastfetch-linux-${ARCH_UNAME/x86_64/amd64}.deb"; then
            sudo dpkg -i "$ff_tmp/fastfetch.deb" &> /dev/null || FAILED_PACKAGES+=(fastfetch)
        else
            FAILED_PACKAGES+=(fastfetch)
        fi
        rm -rf "$ff_tmp"
    fi

    # ── GNOME desktop ─────────────────────────────────────────
    #
    # The macOS-inspired desktop look. This installs the *parts*; all of the
    # configuration lives in `desktop-theme`, which chezmoi deploys and the
    # post-install step below runs. Skipped on machines with no GNOME session
    # (servers, containers, another desktop).

    if command -v gnome-shell &> /dev/null; then
        echo ""
        echo "Installing GNOME desktop components..."

        install_optional_pkg gnome-shell-extension-manager
        install_optional_pkg gnome-tweaks
        # GNOME can take a screenshot but cannot annotate one.
        install_optional_pkg flameshot
        install_optional_pkg dconf-cli
        install_optional_pkg imagemagick
        install_optional_pkg fonts-inter
        install_optional_pkg x11-xserver-utils

        GNOME_SHELL_VERSION="$(gnome-shell --version | grep -oE '[0-9]+' | head -1)"

        # extensions.gnome.org serves a zip built for one shell version, so the
        # version has to be asked for by number rather than guessed.
        install_gnome_extension() {
            local uuid="$1"
            [ ! -d "$HOME/.local/share/gnome-shell/extensions/$uuid" ] || return 0
            echo "Installing GNOME extension: $uuid"
            local tmp info url
            tmp="$(mktemp -d)" || { FAILED_PACKAGES+=("gnome-ext:$uuid"); return; }
            info="$(curl -fsSL "https://extensions.gnome.org/extension-info/?uuid=${uuid/@/%40}&shell_version=$GNOME_SHELL_VERSION" 2> /dev/null)"
            url="$(printf '%s' "$info" | grep -o '"download_url": "[^"]*"' | head -1 | sed 's/.*"download_url": "//; s/"$//')"
            if [ -z "$url" ] \
                || ! curl -fsSL "https://extensions.gnome.org$url" -o "$tmp/ext.zip" \
                || ! gnome-extensions install --force "$tmp/ext.zip" &> /dev/null; then
                FAILED_PACKAGES+=("gnome-ext:$uuid")
            fi
            rm -rf "$tmp"
        }

        install_gnome_extension blur-my-shell@aunetx
        install_gnome_extension just-perfection-desktop@just-perfection
        install_gnome_extension rounded-window-corners@fxgn
        install_gnome_extension search-light@icedman.github.com
        install_gnome_extension tilingshell@ferrarodomenico.com

        # Icon theme: Colloid -- macOS-adjacent shapes without being a clone.
        if [ ! -d "$HOME/.local/share/icons/Colloid-Dark" ]; then
            echo "Installing Colloid icon theme..."
            colloid_tmp="$(mktemp -d)"
            if git clone --depth=1 -q https://github.com/vinceliuice/Colloid-icon-theme.git \
                "$colloid_tmp/colloid"; then
                (cd "$colloid_tmp/colloid" && ./install.sh -d "$HOME/.local/share/icons" -s default) &> /dev/null \
                    || FAILED_PACKAGES+=(colloid-icon-theme)
            else
                FAILED_PACKAGES+=(colloid-icon-theme)
            fi
            rm -rf "$colloid_tmp"
        fi

        # Cursor theme: Bibata Modern Classic -- a black arrow with a white
        # outline, the shape macOS uses, drawn for modern GNOME sizes.
        if [ ! -d "$HOME/.local/share/icons/Bibata-Modern-Classic" ]; then
            echo "Installing Bibata cursor theme..."
            bibata_tmp="$(mktemp -d)"
            if curl -fsSL -o "$bibata_tmp/bibata.tar.xz" \
                "https://github.com/ful1e5/Bibata_Cursor/releases/download/$BIBATA_VERSION/Bibata-Modern-Classic.tar.xz"; then
                mkdir -p "$HOME/.local/share/icons"
                tar -xJf "$bibata_tmp/bibata.tar.xz" -C "$HOME/.local/share/icons" \
                    || FAILED_PACKAGES+=(bibata-cursor-theme)
            else
                FAILED_PACKAGES+=(bibata-cursor-theme)
            fi
            rm -rf "$bibata_tmp"
        fi
    fi
fi

# ── 5. Post-install setup ─────────────────────────────────────

echo ""
echo "Running post-install setup..."

# Install Slack's official CLI for building and managing Slack apps. The
# installer is shared by macOS and Linux and places the command in a
# user-scoped PATH location, so no sudo access is required.
if ! command -v slack &> /dev/null; then
    echo "Installing Slack CLI..."
    slack_tmp="$(mktemp -d)"
    if curl -fsSL https://downloads.slack-edge.com/slack-cli/install.sh -o "$slack_tmp/install.sh"; then
        bash "$slack_tmp/install.sh" || FAILED_PACKAGES+=(slack-cli)
    else
        FAILED_PACKAGES+=(slack-cli)
    fi
    rm -rf "$slack_tmp"
else
    echo "Slack CLI already installed."
fi

# Clone TPM (Tmux Plugin Manager)
if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    echo "Installing TPM (Tmux Plugin Manager)..."
    git clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
else
    echo "TPM already installed."
fi

# Install the plugins .tmux.conf declares. TPM documents this as `prefix + I'
# inside a running session, which is easy to never get round to -- and a
# machine that skips it gets no clipboard yank (tmux-yank) and no session
# save/restore (tmux-resurrect), with nothing on screen to explain why.
# install_plugins is the same operation headless, and is a no-op for plugins
# that are already cloned. Dotfiles were applied in step 3, so .tmux.conf is
# in place by now.
if [ -x "$HOME/.tmux/plugins/tpm/bin/install_plugins" ]; then
    echo "Installing tmux plugins..."
    "$HOME/.tmux/plugins/tpm/bin/install_plugins" || FAILED_PACKAGES+=("tmux plugins")
fi

# Clone the home-grown editor packages.  These are NOT chezmoi-managed: the
# Emacs `use-package` forms (langs.el) and the Neovim spec (exact_plugins/
# lectern.lua) are guarded on these directories, so a machine without them
# still starts cleanly — cloning them here is what turns the features on.
# HTTPS, not SSH: this runs before `gh auth login` on a fresh machine.
for repo in elisp/fp-repl elisp/dune-transient elisp/mli-lens nvim/lectern.nvim; do
    dest="$HOME/workspace/$repo"
    name="${repo##*/}"
    if [ ! -d "$dest" ]; then
        echo "Cloning $name..."
        mkdir -p "$(dirname "$dest")"
        git clone --quiet "https://github.com/naamanu/$name.git" "$dest" \
            || FAILED_PACKAGES+=("$name")
    fi
done

# Neovim Python provider. Pin the interpreter: without --python, uv picks the
# system python (3.9 on macOS), which is too old for jupyter_client.
if command -v uv &> /dev/null; then
    echo "Setting up Neovim Python provider..."
    uv venv --python 3.12 ~/.local/share/nvim/venv || FAILED_PACKAGES+=(nvim-venv)
    uv pip install --python ~/.local/share/nvim/venv/bin/python pynvim jupyter_client || FAILED_PACKAGES+=(pynvim)
fi

# ML/AI & Scientific Python tools (global uv tools)
if command -v uv &> /dev/null; then
    echo "Installing ML/AI & Scientific Python tools..."
    uv tool install jupyterlab --with ipykernel --with jupyterlab-vim || true
    uv tool install ipython || true
    uv tool install mlflow || true
    uv tool install dvc || true
    uv tool install tensorboard || true
fi

# Python editor tools shared by Emacs and Neovim.  basedpyright is a PyPI
# package too, so it lives with the other uv tools rather than in the npm
# prefix; jupytext lets Emacs code-cells open .ipynb files as scripts.
if command -v uv &> /dev/null; then
    uv tool install basedpyright || FAILED_PACKAGES+=(basedpyright)
    uv tool install jupytext || FAILED_PACKAGES+=(jupytext)
    # CMake language server for the systems track (both editors gate on it).
    uv tool install cmake-language-server || FAILED_PACKAGES+=(cmake-language-server)
fi

# Install Node.js LTS via fnm
if command -v fnm &> /dev/null; then
    echo "Installing Node.js LTS via fnm..."
    fnm install --lts || FAILED_PACKAGES+=(node)
    fnm default lts-latest 2>/dev/null || true
    # Put node/npm on this script's PATH so the npm tools below install.
    eval "$(fnm env --shell bash)" || true
fi

# User-scoped fallbacks keep Ubuntu usable when optional distro packages are
# unavailable or sudo is not available during a later repair run.
if ! command -v direnv &> /dev/null && command -v go &> /dev/null; then
    GOBIN="$HOME/.local/bin" go install github.com/direnv/direnv/v2@latest || FAILED_PACKAGES+=(direnv)
fi
if ! command -v shfmt &> /dev/null && command -v go &> /dev/null; then
    GOBIN="$HOME/.local/bin" go install mvdan.cc/sh/v3/cmd/shfmt@latest || FAILED_PACKAGES+=(shfmt)
fi

if ! command -v rustup &> /dev/null; then
    rustup_tmp="$(mktemp -d)"
    if curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o "$rustup_tmp/rustup-init.sh"; then
        sh "$rustup_tmp/rustup-init.sh" -y --profile minimal || FAILED_PACKAGES+=(rustup)
    else
        FAILED_PACKAGES+=(rustup)
    fi
    rm -rf "$rustup_tmp"
fi
export PATH="$HOME/.cargo/bin:$PATH"

if [ "$PLATFORM" = linux ] && ! command -v lua-language-server &> /dev/null; then
    mkdir -p "$HOME/.local/opt/lua-language-server"
    luals_tmp="$(mktemp -d)"
    if curl -fsSL "https://github.com/LuaLS/lua-language-server/releases/download/$LUA_LS_VERSION/lua-language-server-$LUA_LS_VERSION-linux-${NODE_ARCH}.tar.gz" -o "$luals_tmp/lua-language-server.tar.gz"; then
        tar xzf "$luals_tmp/lua-language-server.tar.gz" -C "$HOME/.local/opt/lua-language-server"; rm -rf "$luals_tmp"
        ln -sf "$HOME/.local/opt/lua-language-server/bin/lua-language-server" "$HOME/.local/bin/lua-language-server"
    else
        FAILED_PACKAGES+=(lua-language-server)
    fi
fi

if [ "$PLATFORM" = linux ] && ! command -v ghcup &> /dev/null; then
    ghcup_tmp="$(mktemp -d)"
    if curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org -o "$ghcup_tmp/ghcup.sh"; then
        BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh "$ghcup_tmp/ghcup.sh" || FAILED_PACKAGES+=(ghcup)
    else
        FAILED_PACKAGES+=(ghcup)
    fi
    rm -rf "$ghcup_tmp"
fi
export PATH="$HOME/.ghcup/bin:$PATH"
if command -v ghcup &> /dev/null; then
    ghcup install ghc recommended || true
    ghcup set ghc recommended || true
    ghcup install cabal recommended || true
    ghcup set cabal recommended || true
    ghcup install hls recommended || FAILED_PACKAGES+=(haskell-language-server)
    ghcup set hls recommended || true
fi
if ! command -v ormolu &> /dev/null && command -v cabal &> /dev/null; then
    cabal update || true
    cabal install ormolu --installdir="$HOME/.local/bin" --overwrite-policy=always || FAILED_PACKAGES+=(ormolu)
fi

if ! command -v lldb-dap &> /dev/null; then
    LLDB_DAP="$(command -v lldb-dap-19 || command -v lldb-dap-18 || true)"
    [ -z "$LLDB_DAP" ] || ln -sf "$LLDB_DAP" "$HOME/.local/bin/lldb-dap"
fi

# Shared editor tools live in a fixed user prefix so Emacs and Neovim resolve
# the same binaries regardless of the active Node version manager.
if command -v npm &> /dev/null; then
    npm install -g --prefix "$HOME/.local" \
        typescript prettier eslint @vtsls/language-server \
        vscode-langservers-extracted yaml-language-server \
        dockerfile-language-server-nodejs bash-language-server \
        sql-formatter || FAILED_PACKAGES+=(node-editor-tools)
fi

# Racket language server for Neovim (Emacs uses racket-mode's own back end).
if command -v raco &> /dev/null; then
    raco pkg install --auto --skip-installed --batch racket-langserver || FAILED_PACKAGES+=(racket-langserver)
fi

if command -v go &> /dev/null; then
    GOBIN="$HOME/.local/bin" go install golang.org/x/tools/gopls@latest || FAILED_PACKAGES+=(gopls)
    GOBIN="$HOME/.local/bin" go install github.com/go-delve/delve/cmd/dlv@latest || FAILED_PACKAGES+=(delve)
    GOBIN="$HOME/.local/bin" go install golang.org/x/tools/cmd/goimports@latest || FAILED_PACKAGES+=(goimports)
fi

if command -v rustup &> /dev/null; then
    rustup component add rust-analyzer rustfmt clippy || FAILED_PACKAGES+=(rust-components)
fi

if command -v cargo &> /dev/null && ! command -v stylua &> /dev/null; then
    cargo install stylua || FAILED_PACKAGES+=(stylua)
fi

# Set fish as default shell
FISH_PATH="$(command -v fish 2>/dev/null || true)"
if [ -n "$FISH_PATH" ]; then
    if [ "$PLATFORM" = "mac" ]; then
        CURRENT_SHELL="$(dscl . -read /Users/"$USER" UserShell 2>/dev/null | awk '{print $2}' || true)"
    else
        CURRENT_SHELL="$(getent passwd "$USER" 2>/dev/null | cut -d: -f7 || true)"
    fi
    if [ "$CURRENT_SHELL" != "$FISH_PATH" ]; then
        echo "Setting fish as default shell..."
        if ! grep -qxF "$FISH_PATH" /etc/shells 2>/dev/null; then
            echo "$FISH_PATH" | sudo tee -a /etc/shells > /dev/null
        fi
        chsh -s "$FISH_PATH"
    else
        echo "fish is already the default shell."
    fi
else
    echo "Warning: fish not found. Skipping default shell change."
fi

# Apply the desktop look. `desktop-theme` was deployed by chezmoi in step 3;
# it is idempotent, and `desktop-theme --reset` undoes it.
if [ "$PLATFORM" = "linux" ] && command -v gnome-shell &> /dev/null \
    && [ -x "$HOME/.local/bin/desktop-theme" ]; then
    echo ""
    echo "Applying the desktop theme..."
    "$HOME/.local/bin/desktop-theme" || FAILED_PACKAGES+=("desktop-theme")
fi

# ── 6. Summary ─────────────────────────────────────────────────

echo ""
echo "========================================="
echo "  Bootstrap complete!"
echo "========================================="
echo ""
echo "What was done:"
echo "  - chezmoi installed and dotfiles deployed"
echo "  - Development tools installed ($PLATFORM)"
echo "  - TPM (Tmux Plugin Manager) and tmux plugins installed"
echo "  - Home-grown Emacs/Neovim packages cloned into ~/workspace"
echo "  - Default shell set to fish"
if [ "$PLATFORM" = "linux" ] && command -v gnome-shell &> /dev/null; then
    echo "  - GNOME desktop themed (desktop-theme; --reset to undo)"
fi

if [ ${#FAILED_PACKAGES[@]} -gt 0 ]; then
    echo ""
    echo "Failed to install:"
    for pkg in "${FAILED_PACKAGES[@]}"; do
        echo "  - $pkg"
    done
fi

echo ""
echo "Manual next steps:"
step=0
next_step() { step=$((step + 1)); echo "  $step. $1"; }
next_step "Restart your terminal (or log out and back in)"
next_step "Install Neovim plugins:  nvim '+Lazy sync' +qa"
if [ "$PLATFORM" = "linux" ] && command -v gnome-shell &> /dev/null; then
    next_step "Log out and back in so the GNOME extensions load"
fi
if [ "$PLATFORM" = "linux" ] && [ "$INSTALL_DOCKER" = "1" ]; then
    next_step "Log out/in for Docker group changes"
fi
if [ "$INSTALL_DOCKER" != "1" ]; then
echo ""
echo "Docker was skipped. To include it, re-run with INSTALL_DOCKER=1."
fi
echo ""
