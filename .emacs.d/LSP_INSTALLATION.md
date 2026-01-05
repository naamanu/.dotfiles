# LSP Server Installation Guide

## ✅ Already Installed (Essential)

The following LSP servers are now installed and ready to use:

- ✅ **typescript-language-server** - JavaScript/TypeScript
- ✅ **bash-language-server** - Bash scripts
- ✅ **vscode-langservers-extracted** - JSON, CSS, HTML, ESLint
- ✅ **yaml-language-server** - YAML files

## 🚀 Quick Test

Restart Emacs and try opening these files:

```bash
# Test TypeScript/JavaScript LSP
emacs test.ts
emacs test.js

# Test Bash LSP
emacs test.sh

# Test JSON LSP
emacs test.json

# Test YAML LSP
emacs test.yaml
```

You should see **Eglot** in the modeline when LSP connects!

---

## 📦 Install More LSP Servers (Optional)

### Rust

```bash
rustup component add rust-analyzer
```

### Go

```bash
brew install gopls
# or
go install golang.org/x/tools/gopls@latest
```

### Python

```bash
pip3 install ruff-lsp
```

### OCaml

```bash
opam install ocaml-lsp-server
```

### Haskell

```bash
brew install haskell-language-server
# or via ghcup
ghcup install hls
```

### Functional Languages

#### Clojure

```bash
brew install clojure-lsp/brew/clojure-lsp-native
```

#### Elixir

```bash
brew install elixir-ls
```

#### Elm

```bash
npm install -g @elm-tooling/elm-language-server
```

#### Gleam

```bash
cargo install gleam
```

#### Nix

```bash
brew install nil
# or
cargo install nil
```

#### PureScript

```bash
npm install -g purescript-language-server
```

---

## 🤖 Automated Installation

### Option 1: Essential Only (Already Done!)

```bash
cd ~/.emacs.d
./install-essential-lsp.sh
```

### Option 2: All LSP Servers

```bash
cd ~/.emacs.d
./install-lsp-servers.sh
```

This will install:

- All npm-based servers
- Cargo-based servers (Gleam, Rust)
- System package manager servers (Homebrew on macOS)

---

## 🔍 Verify Installation

Check which LSP servers are available:

```bash
# TypeScript
which typescript-language-server

# Bash
which bash-language-server

# JSON/HTML/CSS
which vscode-json-language-server

# YAML
which yaml-language-server

# Rust
which rust-analyzer

# Go
which gopls

# Python
which ruff-lsp

# OCaml
which ocamllsp

# Haskell
which haskell-language-server

# Clojure
which clojure-lsp

# Elm
which elm-language-server

# Gleam
which gleam

# Nix
which nil
```

---

## 📝 LSP Configuration in Emacs

Your LSP configuration is in: `~/.emacs.d/elisp/plugins/el-lsp.el`

### Adding a New LSP Server

If you install a new LSP server, add it to `el-lsp.el`:

```elisp
(add-to-list 'eglot-server-programs
  '(your-mode . ("your-lsp-server" "args")))
```

Example for Zig:

```elisp
(add-to-list 'eglot-server-programs
  '(zig-mode . ("zls")))
```

---

## 🐛 Troubleshooting

### LSP Not Connecting

1. **Check if server is installed**

   ```bash
   which <server-name>
   ```

2. **Check if it's in PATH**

   ```bash
   echo $PATH
   ```

3. **Check Eglot status**

   ```elisp
   M-x eglot
   M-x eglot-reconnect
   ```

4. **View Eglot events**
   ```elisp
   M-x eglot-events-buffer
   ```

### Server Crashes

Check Eglot stderr:

```elisp
M-x eglot-stderr-buffer
```

### Wrong Server Version

Some servers need specific versions. Check:

```bash
<server-name> --version
```

---

## 💡 Pro Tips

1. **Multiple Projects**: Eglot automatically uses project-specific settings
2. **Performance**: LSP servers cache in `.eglot/` directory
3. **Formatting**: Some LSP servers provide formatting (use `C-c l f`)
4. **Code Actions**: `C-c l a` shows available actions
5. **Diagnostics**: Errors/warnings show automatically in buffer

---

## 📊 Coverage Status

| Language      | LSP Server                  | Status       | Install Command                                   |
| ------------- | --------------------------- | ------------ | ------------------------------------------------- |
| TypeScript/JS | typescript-language-server  | ✅ Installed | Already done                                      |
| Bash          | bash-language-server        | ✅ Installed | Already done                                      |
| JSON          | vscode-json-language-server | ✅ Installed | Already done                                      |
| YAML          | yaml-language-server        | ✅ Installed | Already done                                      |
| Rust          | rust-analyzer               | ⚠️ Optional  | `rustup component add rust-analyzer`              |
| Go            | gopls                       | ⚠️ Optional  | `brew install gopls`                              |
| Python        | ruff-lsp                    | ⚠️ Optional  | `pip3 install ruff-lsp`                           |
| OCaml         | ocamllsp                    | ⚠️ Optional  | `opam install ocaml-lsp-server`                   |
| Haskell       | hls                         | ⚠️ Optional  | `brew install haskell-language-server`            |
| Clojure       | clojure-lsp                 | ⚠️ Optional  | `brew install clojure-lsp`                        |
| Elixir        | elixir-ls                   | ⚠️ Optional  | `brew install elixir-ls`                          |
| Elm           | elm-language-server         | ⚠️ Optional  | `npm install -g @elm-tooling/elm-language-server` |
| Gleam         | gleam                       | ⚠️ Optional  | `cargo install gleam`                             |
| Nix           | nil                         | ⚠️ Optional  | `brew install nil`                                |
| PureScript    | purescript-language-server  | ⚠️ Optional  | `npm install -g purescript-language-server`       |

---

## 🎯 Recommended Priority

Install in this order based on your needs:

### Priority 1 (Web Development) - ✅ Done!

- ✅ typescript-language-server
- ✅ bash-language-server
- ✅ JSON/YAML/CSS/HTML

### Priority 2 (Systems Programming)

- Rust: `rustup component add rust-analyzer`
- Go: `brew install gopls`

### Priority 3 (Functional Programming)

- OCaml: `opam install ocaml-lsp-server`
- Haskell: `brew install haskell-language-server`
- Clojure: `brew install clojure-lsp`

### Priority 4 (Other Functional)

- Elixir, Elm, Gleam, Nix, PureScript (as needed)

---

**Status**: ✅ Essential LSP servers installed and ready!  
**Next**: Restart Emacs and test opening files!
