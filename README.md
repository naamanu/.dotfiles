# Dotfiles

Managed with [chezmoi](https://www.chezmoi.io/). Source directory: `~/.local/share/chezmoi`.

## Managing dotfiles with chezmoi

**Edit a dotfile:**
```sh
chezmoi edit ~/.tmux.conf            # opens the source file in $EDITOR
chezmoi apply                        # deploy changes to $HOME
chezmoi edit --apply ~/.tmux.conf    # edit + apply in one step
```

**Preview changes before applying:**
```sh
chezmoi diff
```

**Add a new file to chezmoi:**
```sh
chezmoi add ~/.config/wezterm/wezterm.lua
```

**Common commands:**

| Command | Description |
| :--- | :--- |
| `chezmoi edit <file>` | Edit the source version of a managed file |
| `chezmoi apply` | Deploy source state to `$HOME` |
| `chezmoi diff` | Preview pending changes |
| `chezmoi add <file>` | Start managing a new file |
| `chezmoi forget <file>` | Stop managing a file |
| `chezmoi managed` | List all managed files |
| `chezmoi cd` | cd into the source directory (for git operations) |
| `chezmoi update` | `git pull` + `apply` in one step |

**Committing changes:**
```sh
chezmoi cd
git add -A && git commit -m "message" && git push
exit
```

**Setting up on a new machine (full bootstrap — chezmoi + tools + post-setup):**
```sh
bash <(curl -s https://raw.githubusercontent.com/naamanu/.dotfiles/main/setup.sh)
```

Or deploy dotfiles only (no tool installation):
```sh
chezmoi init --apply https://github.com/naamanu/.dotfiles.git
```

Use the full URL, not the `chezmoi init --apply naamanu` shorthand — that
shorthand expands to `naamanu/dotfiles`, a different repo. With a GitHub SSH
key already set up, `git@github.com:naamanu/.dotfiles.git` works too.

See [INSTALL.md](INSTALL.md) for details and manual alternatives.

## Editor docs

- [Shared Emacs/Neovim capability contract](docs/capability-parity.md)

- [Emacs setup overview](docs/emacs.md)
- [Emacs development workflow](docs/emacs-dev-workflow.md)
- [Emacs Org workflow](docs/emacs-org-workflow.md)
- [Neovim setup overview](docs/nvim.md)
- [Neovim tutorials](docs/nvim-tutorials.md)
- [tmux setup overview](docs/tmux.md)
- [Desktop setup (GNOME)](docs/desktop.md)

## Desktop (Linux/GNOME)

A macOS-inspired GNOME desktop — floating bottom dock, translucency, rounded
corners, thin top bar, Spotlight-style launcher — with the stock shell theme
left in place, so a GNOME upgrade cannot break it.

```sh
desktop-theme                  # apply the look
desktop-theme --check          # what is installed
desktop-theme --reset          # back to Ubuntu defaults
desktop-wallpaper --list       # the generated gradient wallpapers
```

Two overlays come with it, on the Omarchy pattern:

```sh
menu                           # Super+Alt+Space -- theme, wallpaper, settings
keys                           # Super+K -- searchable keybinding cheatsheet
theme-mode toggle              # light/dark across every surface, desktop included
```

`setup.sh` installs the parts; `desktop-theme` writes the configuration. All of
it is Linux-only — macOS is excluded in `.chezmoiignore`. See
[docs/desktop.md](docs/desktop.md).

## Emacs machine setup

Emacs is a single, near-stock `~/.emacs.d/init.el` (evil, vertico/consult,
Haskell and OCaml tooling, Denote + Org notes). See `docs/emacs.md`.

---

# Keybindings Documentation

This document outlines the keybindings configured for Neovim, Emacs, and tmux.

---

## Neovim Keybindings

Neovim is near-stock (built-in LSP, completion, snippets, comments, undotree;
five plugins via `vim.pack`). The full key list is in `docs/nvim.md`.

---

## AeroSpace Keybindings (macOS Tiling WM)

**Modifier:** `Alt`

### Focus & Move

| Keybinding       | Description              |
| :--------------- | :----------------------- |
| `Alt + h/j/k/l`       | Focus left/down/up/right |
| `Alt + Shift + h/j/k/l` | Move window left/down/up/right |

### Workspaces

| Keybinding       | Description              |
| :--------------- | :----------------------- |
| `Alt + 1–6`           | Switch to workspace 1–6 |
| `Alt + Shift + 1–6`   | Move window to workspace 1–6 |

### Layout

| Keybinding       | Description              |
| :--------------- | :----------------------- |
| `Alt + /`              | Toggle horizontal/vertical tiling |
| `Alt + ,`              | Toggle accordion layout  |
| `Alt + f`              | Toggle fullscreen        |
| `Alt + Shift + f`      | Toggle floating/tiling   |
| `Alt + v`              | Split horizontal         |
| `Alt + b`              | Split vertical           |
| `Alt + Shift + -/=`    | Resize smaller/larger    |
| `Alt + Shift + c`      | Reload config            |
| `Alt + Shift + ;`      | Enter service mode       |

---

## Emacs Keybindings

Stock Emacs keys plus evil and a small `SPC` leader; see `docs/emacs.md`.

---

## Shell Keybindings

### Aliases & Functions

Abbreviations expand visibly at the prompt (interactive shells only).

| Abbr    | Expands to                 |
| :------ | :------------------------- |
| `ls` `ll` `la` `lt` `tree` | `eza` variants (`--icons`, `-l`, `-la`, `--tree`) |
| `cat`   | `bat`                      |
| `grep`  | `rg`                       |
| `g`     | `git`                      |
| `..` `...` `....` | `cd ..` / `cd ../..` / `cd ../../..` |
| `wks` `work` | `cd ~/workspace` / `cd ~/workspace/work` |
| `c` `h` | `clear` / `history`        |
| `vim` `v` | `nvim`                   |
| `jl`    | `jupyter-lab`              |
| `tb`    | `tensorboard --logdir`     |

**Navigation**

| Function | Description |
| :------- | :---------- |
| `mkcd <dir>` | Create a directory (with parents) and cd into it |
| `up [n\|name]` | Up one level, up `n` levels, or to the nearest ancestor named `name` |
| `cdr` | cd to the root of the current git repository |
| `scratch [name]` | cd into a dated throwaway dir under `~/scratch` |

**Git**

| Function | Description |
| :------- | :---------- |
| `clone <url> [dir]` | git clone and cd into the result |
| `gwt [branch]` | List worktrees, or add one for `branch` and cd into it |

**Utilities**

| Function | Description |
| :------- | :---------- |
| `dotenv [file]` | Load a `.env` into the shell (fish cannot `source` one) |
| `extract <archive>` | Extract any common archive format |
| `bak <file>` | Timestamped backup copy beside the original |

**Projects & tooling**

| Function | Description |
| :------- | :---------- |
| `dev` | tmux dev session: nvim left, claude right |
| `mlenv <name> [type]` | Bootstrap a uv ML project (`dl`, `ml`, `llm`) |
| `jk` | Register current uv venv as a Jupyter kernel |
| `ocaml-new <name>` | Bootstrap a dune project and stamp `.ocamlformat` |
| `c-new <name>` | Create and enter a C17 CMake project |
| `cpp-new <name>` | Create and enter a C++20 CMake project; `make` builds and runs, `make build` only compiles |
| `c-run [args...]` | Configure, build, and run a C/C++ starter project from its root |
| `ocamlformat-init` | Stamp the global ocamlformat config into this project |

**Shell integrations:** atuin (history search with `Ctrl+R`), zoxide (`z`/`cd`), starship prompt.

### Modern CLI Tools

| Tool | Replaces | Description |
| :--- | :------- | :---------- |
| yazi | ranger/nnn | Terminal file manager with image preview |
| atuin | `Ctrl+R` | Synced shell history search across machines |
| glow | — | Render markdown in the terminal |
| dust | du | Disk usage visualizer |
| procs | ps | Modern process viewer |
| hyperfine | time | Command benchmarking |
| tokei | cloc | Code statistics by language |

---

## tmux Keybindings

**Prefix Key:** The tmux prefix key is set to `C-a`.

| Keybinding       | Description                                    |
| :--------------- | :--------------------------------------------- |
| `C-a + R`        | Reload tmux configuration                      |
| `C-a + \|`       | Split window horizontally                      |
| `C-a + -`        | Split window vertically                        |
| `C-a + h/j/k/l`  | Select left/down/up/right pane (vim-like)      |
| `C-a + H/J/K/L`  | Resize left/down/up/right pane                 |
| `C-a + m`        | Maximize pane (toggle zoom)                    |
| `C-a + p`        | Previous window                                |
| `C-a + n`        | Next window                                    |
| `C-h/j/k/l`      | Move between tmux panes and Neovim windows      |
| `M-h/j/k/l`      | Resize pane (no prefix)                        |
| `C-a + C-l`      | Clear screen (C-l is taken by navigation)      |
| `C-a + [`        | Enter copy mode                                |
| `v`              | Begin selection (in copy mode)                 |
| `C-v`            | Rectangle selection (in copy mode)             |
| `y`              | Copy selection and exit (in copy mode)         |
| `C-a + U`        | Update plugins (TPM; install is automatic)     |
| `C-a + C-s`      | Save session now (continuum saves every 15m)   |
| `C-a + C-r`      | Restore session (automatic on server start)    |
