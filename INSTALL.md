# Installation Guide

## One-Command Setup

```sh
bash <(curl -s https://raw.githubusercontent.com/naamanu/.dotfiles/main/setup.sh)
```

This single command will:

1. **Install chezmoi** (dotfile manager)
2. **Clone and deploy dotfiles** (SSH when a GitHub key is set up, HTTPS otherwise)
3. **Install development tools** (platform-appropriate)
4. **Post-install setup**: TPM and the tmux plugins it manages, set fish as default shell

The script is safe to re-run on an already-configured machine: every step checks for the tool first and collects failures into a summary instead of aborting.

## What's Managed by Chezmoi

Chezmoi manages all configuration files. The setup script only installs packages — it never writes config files.

| Config | Source path in chezmoi |
| :--- | :--- |
| fish shell | `dot_config/fish/` |
| ghostty | `dot_config/ghostty/` |
| aerospace | `dot_config/aerospace/` |
| starship | `dot_config/starship.toml` |
| neovim | `dot_config/nvim/` |
| tmux | `dot_tmux.conf`, `dot_config/tmux/` (palettes) |
| git | `dot_gitconfig.tmpl`, `dot_gitignore_global` |
| emacs | `dot_emacs.d/` |
| ocamlformat | `dot_config/ocamlformat` |
| zsh login shell (macOS) | `dot_zprofile` |
| Claude Code / Codex | `dot_claude/`, `dot_codex/` |
| bin scripts | `private_dot_local/private_bin/` (`theme-mode`, `dev-doctor`, `keys`, `menu`, desktop scripts) |

To edit a config: `chezmoi edit ~/.config/fish/config.fish --apply`

## What Gets Installed

### Both Platforms
- Core: build tools, git, curl, wget, cmake
- Shell: fish, tmux, starship, zoxide, xsel (clipboard backend for tmux-yank;
  wl-clipboard instead on a Wayland session — tmux-yank prefers wl-copy over
  xsel without checking the session type, so only one of the two is installed)
- CLI: neovim, ripgrep, fd, fzf, bat, eza, jq, btop, yazi, atuin, glow, dust, procs, hyperfine, tokei, Slack CLI
  (on Debian/Ubuntu, neovim and the tree-sitter CLI come from official GitHub
  release binaries into `~/.local` — the apt packages are too old for this
  config, which needs Neovim ≥ 0.10 and tree-sitter CLI ≥ 0.22; Emacs likewise
  comes from the classic snap, since apt ships 29 and the config targets 30)
- Languages: Node.js (fnm), Python (uv), Rust (rustup, with rust-analyzer/clippy/rustfmt), Go, Lua
- C/C++: clang/clangd, clang-format, bear, lldb
- FP: OCaml (opam, dune, merlin, ocaml-lsp, utop), Haskell (ghc, cabal, HLS, ormolu),
  Racket (minimal-racket), Standard ML (SML/NJ, millet), Lisp (sbcl)
- ML/AI & Scientific: jupyterlab and ipython (uv tools), pandoc, typst, ollama, dvisvgm (Org LaTeX previews)
- MLOps (via uv tool): mlflow, dvc, tensorboard
- LSP/Formatters: language servers, prettier, sql-formatter, stylua, ruff, basedpyright, jupytext and cmake-language-server (uv tools), texlab (macOS), shellcheck, enchant (spellcheck)
- Emacs pdf-tools build deps: poppler, automake
- Treesitter: tree-sitter CLI (parser compilation for Neovim)
- Git: gh, lazygit, git-delta
- Databases: PostgreSQL, SQLite
- Containers: Docker + lazydocker — **opt-in**, set `INSTALL_DOCKER=1`
- Fonts: Iosevka Comfy (editor and terminal face), Symbols Nerd Font (icons), plus JetBrains Mono, Fira Code, Hack, Inconsolata and Commit Mono Nerd Fonts as Emacs fontaine presets
- Home-grown editor packages (cloned into `~/workspace`, not chezmoi-managed):
  `fp-repl`, `dune-transient`, `mli-lens` (Emacs, `~/workspace/elisp/`) and
  `lectern.nvim` (Neovim, `~/workspace/nvim/`).  The editor config is guarded
  on these directories, so both editors start fine without them — the features
  simply stay off.

### macOS Only
- Homebrew (installed automatically)
- Emacs: `emacs-app` cask (native-comp build), linked into `~/.local/bin` as `emacs`/`emacsclient`
- GNU coreutils (`gls` for Emacs Dired)
- Cloud/DevOps: awscli, terraform, kubectl, k9s
- LaTeX: MacTeX (mactex-no-gui)
- Window management: AeroSpace (tiling WM)
- Menu bar: Ice
- Apps: Ghostty (cask), Raycast

## Post-Install Checklist

- [ ] Restart terminal (or log out/in)
- [ ] `nvim '+Lazy sync' +qa` — install Neovim plugins
- [ ] Log out/in for Docker group changes (Linux, only if `INSTALL_DOCKER=1`)
- [ ] `gh auth login` — authenticate GitHub CLI
- [ ] `slack login` — authorize Slack CLI when developing or managing a Slack app
- [ ] `jupyter-lab --version` — verify the uv-installed JupyterLab works
- [ ] `mlenv test-project ml` — test ML project bootstrapping
- [ ] Linux/GNOME: log out and back in so the desktop extensions load
- [ ] Linux/GNOME: `desktop-theme --check` — confirm every desktop part installed
- [ ] Try `menu` (actions) and `keys` (keybinding cheatsheet)

## Troubleshooting

### macOS: Command not found after installation
```sh
source ~/.zprofile
```

### Linux: Docker permission denied
Log out and back in, then:
```sh
docker run hello-world
```

### Neovim: Plugins not loading
```sh
nvim '+Lazy clean' '+Lazy sync'
```

### tmux: Plugin not loading
Plugins declared with `set -g @plugin` install themselves the next time the
config is read, and the config is re-sourced automatically once the clone
finishes — no `prefix + I`. If one is still missing, run the installer by hand
and reload:
```sh
~/.tmux/plugins/tpm/bin/install_plugins
tmux source-file ~/.tmux.conf
```

### Neovim: `attempt to index field 'uv' (a nil value)` on startup
The running nvim is older than 0.10 (typically the distro package shadowing
the tarball install). Check `nvim --version` and that `~/.local/bin` precedes
`/usr/bin` in `$PATH`; re-run setup.sh to install the official tarball.

## Updating

Pull the latest dotfiles and re-apply:
```sh
chezmoi update
```

Update installed packages:
```sh
# macOS
brew update && brew upgrade

# Ubuntu/Debian
sudo apt update && sudo apt upgrade

# Fedora
sudo dnf upgrade

# Arch
sudo pacman -Syu
```

## Shared editor toolchain

Emacs and Neovim use the same PATH-visible language servers, formatters, linters, and debugger adapters. Neovim does not maintain a private Mason toolchain.

```sh
./setup.sh --check       # read-only bootstrap check
dev-doctor --all        # complete shared capability report
dev-doctor --json       # machine-readable report
```

Resolution order is project environment, `~/.local/bin`, then system PATH. Use project lockfiles and direnv for project-specific versions; global tools follow stable releases while editor plugins remain pinned.
