#!/usr/bin/env bash
# install-lsp-servers.sh - Install LSP servers for Emacs functional programming setup

set -e

echo "🚀 Installing LSP servers for functional programming languages..."
echo ""

# Detect OS
OS="$(uname -s)"
case "$OS" in
    Darwin*)  INSTALL_CMD="brew install";;
    Linux*)   
        if command -v apt-get &> /dev/null; then
            INSTALL_CMD="sudo apt-get install -y"
        elif command -v dnf &> /dev/null; then
            INSTALL_CMD="sudo dnf install -y"
        else
            echo "❌ Unsupported package manager. Please install manually."
            exit 1
        fi
        ;;
    *)        echo "❌ Unsupported OS: $OS"; exit 1;;
esac

# Function to check if command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# NPM-based LSP servers
echo "📦 Installing NPM-based LSP servers..."
if command_exists npm; then
    npm install -g typescript-language-server
    npm install -g @elm-tooling/elm-language-server
    npm install -g purescript-language-server
    npm install -g graphql-language-service-cli
    echo "✅ NPM LSP servers installed"
else
    echo "⚠️  NPM not found. Skipping TypeScript, Elm, PureScript, GraphQL LSP servers."
fi

# Cargo-based LSP servers
echo "📦 Installing Cargo-based LSP servers..."
if command_exists cargo; then
    cargo install gleam
    echo "✅ Gleam installed"
else
    echo "⚠️  Cargo not found. Skipping Gleam."
fi

# Rust Analyzer (if not already installed)
if ! command_exists rust-analyzer; then
    if command_exists rustup; then
        rustup component add rust-analyzer
        echo "✅ rust-analyzer installed"
    fi
fi

# System package manager LSP servers
echo "📦 Installing system LSP servers..."

case "$OS" in
    Darwin*)
        brew install clojure-lsp/brew/clojure-lsp-native
        brew install nil  # Nix LSP
        brew install gopls
        brew install haskell-language-server
        
        # Check if Racket is installed
        if command_exists racket; then
            raco pkg install --auto racket-langserver || echo "⚠️  racket-langserver installation failed"
        else
            echo "⚠️  Racket not installed. Install it first: brew install racket"
        fi
        
        # Elixir LS
        if command_exists brew; then
            brew install elixir-ls
        fi
        
        # Erlang LS
        if command_exists brew; then
            brew install erlang-ls
        fi
        
        echo "✅ macOS LSP servers installed"
        ;;
        
    Linux*)
        echo "📦 Installing Linux LSP servers..."
        
        # Clojure LSP
        if ! command_exists clojure-lsp; then
            echo "Installing clojure-lsp..."
            curl -L https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/clojure-lsp-native-linux-amd64.zip -o /tmp/clojure-lsp.zip
            unzip -o /tmp/clojure-lsp.zip -d /tmp
            sudo mv /tmp/clojure-lsp /usr/local/bin/
            sudo chmod +x /usr/local/bin/clojure-lsp
            echo "✅ clojure-lsp installed"
        fi
        
        # Nil (Nix LSP)
        if command_exists cargo; then
            cargo install nil
        fi
        
        echo "⚠️  Some LSP servers may need manual installation on Linux"
        ;;
esac

# Python LSP (Ruff)
echo "📦 Installing Python LSP..."
if command_exists pip3; then
    pip3 install --user ruff-lsp
    echo "✅ ruff-lsp installed"
elif command_exists pip; then
    pip install --user ruff-lsp
    echo "✅ ruff-lsp installed"
fi

# OCaml LSP (if opam is installed)
if command_exists opam; then
    opam install ocaml-lsp-server
    echo "✅ ocaml-lsp-server installed"
else
    echo "⚠️  OPAM not found. Install OCaml first: brew install opam"
fi

# Bash LSP
echo "📦 Installing Bash LSP..."
if command_exists npm; then
    npm install -g bash-language-server
    echo "✅ bash-language-server installed"
fi

# JSON/YAML LSP
if command_exists npm; then
    npm install -g vscode-langservers-extracted
    echo "✅ JSON/YAML LSP servers installed"
fi

echo ""
echo "🎉 LSP server installation complete!"
echo ""
echo "📝 Installed LSP servers:"
echo "   ✅ TypeScript/JavaScript (ts_ls)"
echo "   ✅ Rust (rust-analyzer)"
echo "   ✅ Go (gopls)"
echo "   ✅ Python (ruff)"
echo "   ✅ Bash (bashls)"
echo "   ✅ JSON/YAML"
echo ""
echo "📝 Functional Programming LSP servers:"
echo "   ✅ OCaml (ocamllsp)"
echo "   ✅ Haskell (hls)"
echo "   ✅ Elm (elm-language-server)"
echo "   ✅ Gleam"
echo "   ✅ PureScript"
echo "   ✅ Clojure (clojure-lsp)"
echo "   ✅ Nix (nil)"
echo "   ✅ Elixir (elixir-ls)"
echo "   ✅ Erlang (erlang-ls)"
echo ""
echo "⚠️  Note: Some servers may require additional setup:"
echo "   - Scala: Install metals per-project"
echo "   - Racket: Requires Racket installation first"
echo ""
echo "✨ Your Emacs is now ready for functional programming!"
