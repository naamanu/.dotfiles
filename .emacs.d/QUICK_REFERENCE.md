# Emacs Quick Reference - Post Modernization

## 🚀 Quick Start

### First Launch

1. Open Emacs - it will auto-install all packages (may take 2-3 minutes)
2. Wait for all packages to install
3. Restart Emacs if prompted

### Install LSP Servers (Optional but Recommended)

```bash
cd ~/.emacs.d
./install-lsp-servers.sh
```

Or install selectively:

```bash
# Essential
npm install -g typescript-language-server bash-language-server

# Functional Programming
cargo install gleam
npm install -g purescript-language-server @elm-tooling/elm-language-server
brew install clojure-lsp nil haskell-language-server
```

---

## ✨ New Features You Now Have

### 1. Which-Key (Keybinding Discovery)

- Press `Space` or `C-c` and wait 0.3s
- See all available keybindings!

### 2. Transparency

- Background matches your terminal
- Neovim-style aesthetic

### 3. Git Gutter

- See + / ~ / - in the fringe for git changes
- Updates every 0.02s

### 4. Enhanced Completion

- Auto-complete after 2 characters
- Fast, Neovim-like behavior

### 5. Session Management

- Close Emacs, sessions auto-save
- Reopen and continue where you left off

### 6. Comprehensive Language Support

30+ treesitter parsers including:

- All functional langs (OCaml, Haskell, Elm, Gleam, PureScript, Clojure, Racket, Elixir, Erlang, Scala, Nix)
- Web (TypeScript, JavaScript, HTML, CSS)
- Systems (Rust, Go, C, C++)

---

## 📋 Essential Keybindings

### General

- `C-x C-f` - Find file
- `C-x C-s` - Save file
- `C-x b` - Switch buffer (with consult)
- `C-x g` - Magit status

### LSP (when in a supported file)

- `C-c l a` - Code actions
- `C-c l f` - Format buffer
- `C-c l r` - Rename symbol
- `C-c l d` - Go to declaration
- `C-c l i` - Go to implementation
- `C-c l t` - Go to type definition
- `C-c l s` - Search symbols

### Navigation

- `M-s` - Search in buffer (consult-line)
- `C-c j` - Jump to char (avy)
- `M-o` - Other window
- `C-c p` - Projectile commands

### Completion

- Type 2+ characters, auto-complete appears
- `C-n` / `C-p` - Next/Previous completion
- `RET` - Accept completion
- `C-g` - Cancel

### Terminal

- `C-` ` - Toggle vterm

---

## 🔧 Configuration Files

### Structure

```
~/.emacs.d/
├── init.el                    # Main entry point
├── elisp/
│   ├── core/
│   │   ├── el-packages.el    # Package management
│   │   ├── el-core.el        # Core settings
│   │   └── el-bindings.el    # Keybindings
│   └── plugins/
│       ├── el-theme.el       # Theme & appearance
│       ├── el-which-key.el   # ✨ NEW
│       ├── el-completion.el  # Completion (Vertico/Corfu)
│       ├── el-dev-tools.el   # Dev tools & git
│       ├── el-lsp.el         # LSP configuration
│       ├── el-languages.el   # Language modes & treesitter
│       ├── el-session.el     # ✨ NEW
│       └── ...
```

### Quick Customization

Want to change something? Edit these files:

- **Scrolloff value**: `el-core.el` → `scroll-margin`
- **Color column**: `el-core.el` → `fill-column`
- **Completion delay**: `el-completion.el` → `corfu-auto-delay`
- **Theme**: `el-theme.el` → change `doom-one` to another
- **Transparency**: `el-theme.el` → comment out transparency lines
- **Add language**: `el-languages.el` → add `use-package` block

---

## 🐛 Troubleshooting

### Packages won't install

```elisp
M-x package-refresh-contents
M-x package-install-selected-packages
```

### LSP not working

1. Check server is installed: `which <server-name>`
2. Check it's in PATH
3. Restart LSP: `M-x eglot-reconnect`

### Treesitter parser missing

```elisp
M-x treesit-install-language-grammar
```

### Transparency not showing

- Terminal must support transparency
- For Ghostty/kitty/Alacritty: Enable transparency in terminal config
- For basic terminals: May not work

### Session not restoring

```bash
# Check if desktop directory exists
ls ~/.emacs.d/desktop/
# Should see emacs.desktop file
```

### Slow startup

```elisp
;; Check startup time
M-x emacs-init-time

;; Profile packages
M-x profiler-start
; Do some work
M-x profiler-report
```

---

## 📊 Comparing with Neovim

### What's the Same Now

✅ Transparency  
✅ Scrolloff behavior  
✅ Color column at 100  
✅ Comprehensive treesitter  
✅ Full LSP support  
✅ Git change indicators  
✅ Modern completion  
✅ Session management  
✅ Which-key discoverability

### What's Different (By Design)

- **Emacs strengths**: Org mode, built-in features, Elisp extensibility
- **Neovim strengths**: Lua config, slightly faster startup
- **Both are excellent**: Use the one that fits your workflow!

---

## 💡 Pro Tips

1. **Learning keybindings**: Use which-key! Press prefix and wait
2. **Finding files**: `C-c p f` (projectile) or `C-x C-f` (find-file)
3. **Git workflow**: `C-x g` for Magit (way better than CLI)
4. **LSP setup**: Open a file, Eglot auto-starts
5. **Completion**: Just start typing, let Corfu do the work
6. **Session workflow**: Just close Emacs, it saves automatically

---

## 🎓 Next Steps

### Explore More

1. Learn Magit: `C-x g` and explore the interface
2. Try Org mode: Create a `.org` file
3. Projectile: `C-c p h` for project help
4. Debugging: `C-c d` prefix for DAP commands

### Optional Enhancements

- Add more themes: Install `modus-themes` or `ef-themes`
- Customize modeline: Already have `doom-modeline`
- Add snippets: Already have `yasnippet`
- Org-roam for notes: `(use-package org-roam)`

### Resources

- [Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [Eglot Manual](https://joaotavora.github.io/eglot/)
- [Magit Manual](https://magit.vc/manual/)
- [Projectile Docs](https://docs.projectile.mx/)

---

**Last Updated**: 2026-01-05  
**Status**: ✅ Fully modernized and ready to use!

Enjoy your modernized Emacs configuration! 🎉
