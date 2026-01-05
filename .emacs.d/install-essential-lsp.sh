#!/usr/bin/env bash
# Quick install essential LSP servers for Emacs

set -e

echo "🚀 Installing Essential LSP Servers..."
echo ""

# Check if npm is available
if ! command -v npm &> /dev/null; then
    echo "❌ npm not found. Please install Node.js first."
    exit 1
fi

echo "📦 Installing npm-based LSP servers..."
npm install -g \
    typescript-language-server \
    typescript \
    bash-language-server \
    vscode-langservers-extracted \
    yaml-language-server

echo ""
echo "✅ Essential LSP servers installed!"
echo ""
echo "Installed:"
echo "  ✅ typescript-language-server (JavaScript/TypeScript)"
echo "  ✅ bash-language-server (Bash)"
echo "  ✅ vscode-langservers-extracted (JSON, CSS, HTML)"
echo "  ✅ yaml-language-server (YAML)"
echo ""
echo "For more LSP servers, run: ~/.emacs.d/install-lsp-servers.sh"
