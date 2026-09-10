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

Machine-local Emacs overrides belong in:

- `~/.emacs.d/local-pre.el`
- `~/.emacs.d/local-post.el`

Starter examples are included in the chezmoi source:

- `dot_emacs.d/local-pre.el.example`
- `dot_emacs.d/local-post.el.example`

Inside Emacs, run `C-c e h` for a quick health check of required and optional external tools.

---

# Keybindings Documentation

This document outlines the keybindings configured for Neovim, Emacs, and tmux.

---

## Neovim Keybindings

**Leader Key:** `<Space>` | **Local Leader:** `,`

### General

| Mode | Keybinding     | Description               |
| :--- | :------------- | :------------------------ |
| n    | `<Esc>`        | Clear search highlights   |
| n    | `<C-h/j/k/l>` | Move to left/down/up/right window |
| n    | `<C-Up>`       | Increase window height    |
| n    | `<C-Down>`     | Decrease window height    |
| n    | `<C-Left>`     | Decrease window width     |
| n    | `<C-Right>`    | Increase window width     |
| n    | `<leader>bn`   | Next buffer               |
| n    | `<leader>bp`   | Previous buffer           |
| n    | `<leader>bd`   | Delete buffer             |
| v    | `<`            | Indent left               |
| v    | `>`            | Indent right              |
| v    | `J`            | Move text down            |
| v    | `K`            | Move text up              |
| n    | `<C-d>`        | Scroll down and center    |
| n    | `<C-u>`        | Scroll up and center      |
| n    | `n`            | Next search result        |
| n    | `N`            | Previous search result    |
| x    | `<leader>p`    | Paste without yanking     |
| n    | `<C-s>`        | Save file                 |
| n    | `<leader>q`    | Quit                      |
| n    | `<leader>sv`   | Split window vertically   |
| n    | `<leader>sh`   | Split window horizontally |
| n    | `<leader>se`   | Make splits equal size    |
| n    | `<leader>sx`   | Close current split       |

### Completion (blink.cmp)

| Mode | Keybinding  | Description                    |
| :--- | :---------- | :----------------------------- |
| i    | `<C-j>`     | Select next completion item    |
| i    | `<C-k>`     | Select previous completion item |
| i    | `<Tab>`     | Snippet forward / select next  |
| i    | `<S-Tab>`   | Snippet backward / select prev |
| i    | `<CR>`      | Confirm completion             |
| i    | `<C-Space>` | Trigger completion             |
| i    | `<C-e>`     | Cancel completion              |
| i    | `<C-b>`     | Scroll docs up                 |
| i    | `<C-f>`     | Scroll docs down               |

**Sources:** LSP, lazydev (Neovim Lua API), path, snippets (friendly-snippets), buffer. Snippets use the native `vim.snippet` engine.

### Editor

| Mode    | Keybinding          | Plugin     | Description                    |
| :------ | :------------------ | :--------- | :----------------------------- |
| n       | `gcc`               | builtin    | Toggle comment on current line |
| v       | `gc`                | builtin    | Toggle line comment            |
| n       | `<leader>uu`        | undotree   | Toggle Undotree                |
| n       | `<leader>z`         | snacks.zen | Toggle Zen Mode                |
| n       | `<leader>j`         | treesj     | Split/join syntax node         |
| n, x, o | `<leader><leader>`  | flash.nvim | Flash jump                     |
| n, x, o | `<leader>fj`        | flash.nvim | Flash Treesitter               |
| o       | `<leader>fr`        | flash.nvim | Remote Flash                   |
| o, x    | `<leader>fR`        | flash.nvim | Treesitter Search              |

### Git

| Mode | Keybinding   | Plugin       | Description       |
| :--- | :----------- | :----------- | :---------------- |
| n    | `]h`         | gitsigns     | Next git hunk     |
| n    | `[h`         | gitsigns     | Previous git hunk |
| n    | `<leader>gs` | gitsigns     | Stage hunk        |
| v    | `<leader>gs` | gitsigns     | Stage hunk (visual) |
| n    | `<leader>gr` | gitsigns     | Reset hunk        |
| v    | `<leader>gr` | gitsigns     | Reset hunk (visual) |
| n    | `<leader>gS` | gitsigns     | Stage buffer      |
| n    | `<leader>gu` | gitsigns     | Undo stage hunk   |
| n    | `<leader>gR` | gitsigns     | Reset buffer      |
| n    | `<leader>gp` | gitsigns     | Preview hunk      |
| n    | `<leader>gb` | gitsigns     | Blame line        |
| n    | `<leader>gd` | gitsigns     | Diff this         |
| n    | `<leader>gD` | gitsigns     | Diff this ~       |
| n    | `<leader>gg` | snacks       | Open LazyGit      |
| n    | `<leader>go` | diffview     | Open Diffview     |
| n    | `<leader>gc` | diffview     | Close Diffview    |
| n    | `<leader>gh` | diffview     | File history      |

### File Explorer (Oil)

| Mode | Keybinding   | Description                 |
| :--- | :----------- | :-------------------------- |
| n    | `-`          | Open parent directory (Oil) |
| n    | `<leader>e`  | Open file explorer (Oil)    |

### Picker (snacks.picker)

| Mode | Keybinding   | Description            |
| :--- | :----------- | :--------------------- |
| n    | `<leader>ff` | Find files             |
| n    | `<leader>fp` | Project files          |
| n    | `<leader>fr` | Recent files           |
| n    | `<leader>fg` | Live grep              |
| n    | `<leader>f/` | Search current buffer  |
| n    | `<leader>f.` | Resume last picker     |
| n, x | `<leader>fc` | Find string under cursor |
| n    | `<leader>fb` | Find buffers           |
| n    | `<leader>fh` | Help tags              |
| n    | `<leader>fk` | Keymaps                |
| n    | `<leader>fd` | Diagnostics            |
| n    | `<leader>fs` | Document symbols       |
| n    | `<leader>fS` | Workspace symbols      |

`vim.ui.select` prompts (code actions, etc.) also render through snacks.picker.

### Treesitter

| Mode          | Keybinding  | Description                      |
| :------------ | :---------- | :------------------------------- |
| Normal/Visual | `<C-space>` | Treesitter selection (via flash) |
| Normal/Visual | `af`        | Select around function           |
| Normal/Visual | `if`        | Select inside function           |
| Normal/Visual | `ac`        | Select around class              |
| Normal/Visual | `ic`        | Select inside class              |
| Normal        | `]f`        | Go to next function start        |
| Normal        | `]c`        | Go to next class start           |
| Normal        | `[f`        | Go to previous function start    |
| Normal        | `[c`        | Go to previous class start       |

### LSP

| Mode | Keybinding   | Description          |
| :--- | :----------- | :------------------- |
| n    | `gd`         | Go to definition     |
| n    | `gD`         | Go to declaration    |
| n    | `gr`         | Go to references     |
| n    | `gi`         | Go to implementation |
| n    | `gt`         | Go to type definition |
| n    | `K`          | Hover documentation  |
| n    | `<leader>la` | Code action          |
| n    | `<leader>lr` | Rename               |
| n    | `<leader>ld` | Line diagnostics     |
| n    | `<leader>ls` | Signature help       |
| n    | `<leader>lh` | Toggle inlay hints   |
| n    | `<leader>lc` | Run code lens        |
| n    | `<leader>lC` | Enable code lens     |
| n    | `<leader>lR` | Restart LSP clients  |

**Inlay hints** are auto-enabled for servers that support them (rust-analyzer, vtsls, lua_ls). Toggle per-buffer with `<leader>lh`. **Code lens** auto-refreshes on `BufEnter` and `InsertLeave`.

**Lazydev** provides full Neovim Lua API completions when editing `*.lua` files — no manual `diagnostics.globals` needed.

### Formatting & Linting

| Mode | Keybinding   | Description              |
| :--- | :----------- | :----------------------- |
| n, v | `<leader>cf` | Format buffer            |
| n    | `<leader>ll` | Trigger linting          |

Format on save is enabled by default.

### Trouble (Diagnostics)

| Mode | Keybinding   | Description                          |
| :--- | :----------- | :----------------------------------- |
| n    | `<leader>xx` | Toggle diagnostics (Trouble)         |
| n    | `<leader>xX` | Buffer diagnostics (Trouble)         |
| n    | `<leader>xQ` | Quickfix list (Trouble)              |
| n    | `<leader>cs` | Symbols (Trouble)                    |
| n    | `<leader>cl` | LSP references/definitions (Trouble) |

### Task Runner (Overseer)

| Mode | Keybinding   | Description              |
| :--- | :----------- | :----------------------- |
| n    | `<leader>or` | Run task                 |
| n    | `<leader>os` | Run package script       |
| n    | `<leader>od` | Run `dev` package script |
| n    | `<leader>ol` | Run `lint` package script |
| n    | `<leader>oy` | Run `typecheck` package script |
| n    | `<leader>of` | Run `format` package script |
| n    | `<leader>op` | Run current Python file  |
| n    | `<leader>oT` | Run Python tests         |
| n    | `<leader>ob` | Build current project    |
| n    | `<leader>on` | Run current project tests |
| n    | `<leader>oC` | Compile current C/C++ file |
| n    | `<leader>ot` | Toggle task panel        |
| n    | `<leader>oa` | Task action              |

**Project-aware helpers:** Node package scripts, Python (`uv run python`, pytest), C/C++ (`cmake`, `meson`, `make`), Rust (`cargo`), OCaml (`dune`), and Haskell (`cabal`, `stack`).

### Snacks (Notifications & Utilities)

| Mode | Keybinding   | Description              |
| :--- | :----------- | :----------------------- |
| n    | `<leader>uh` | Notification history     |
| n    | `<leader>ud` | Dismiss notifications    |
| n    | `<leader>rf` | Rename file              |
| n, t | `]]`         | Next reference           |
| n, t | `[[`         | Previous reference       |

**Auto-enabled:** bigfile detection, indent guides, scope highlighting, quickfile, input UI, picker (`vim.ui.select`).

### Terminal agent

`<leader>aa` opens the shared `DEV_AGENT` (Codex, then Claude by default) at the project root; `<leader>ac` copies a file-and-line context reference. See the capability contract for details.

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

### General

| Keybinding | Description                   |
| :--------- | :---------------------------- |
| `ESC`      | Keyboard escape/quit          |
| `M-o`      | Switch to other window        |
| `C-c w`    | Delete window                 |
| `C-c r`    | Replace string                |
| `C-.`      | Embark actions menu           |
| `M-.`      | Embark dwim (do what I mean)  |
| `C-h B`    | Embark bindings               |
| `C-x C-s`  | Save file                     |
| `C-x C-f`  | Find file                     |
| `C-x b`    | Switch buffer (consult)       |
| `C-g`      | Cancel/quit current operation |

### Search & Navigation

| Keybinding | Description                     |
| :--------- | :------------------------------ |
| `M-s l`    | Consult line search             |
| `M-s r`    | Consult ripgrep                 |
| `M-s f`    | Consult find file               |
| `M-y`      | Consult yank from kill ring     |
| `M-g g`    | Consult go to line              |
| `M-g i`    | Consult imenu                   |
| `C-c j`    | Avy jump to character           |
| `M-$`      | Jinx spell-correct              |

### Project.el

**Prefix**: `C-c p` opens the project command map

| Keybinding | Description              |
| :--------- | :----------------------- |
| `C-c p f`  | Find file in project     |
| `C-c p p`  | Switch project           |
| `C-c p P`  | Switch project and find file |
| `C-c p b`  | Switch project buffer    |
| `C-c p D`  | Open project Dired       |
| `C-c p s`  | Search in project (grep) |
| `C-c p m`  | Compile/build project    |
| `C-c p t`  | Run project tests        |
| `C-c p v`  | Open project vterm       |

### LSP (Eglot)

Active in `eglot-mode-map` when an LSP server is attached.

| Keybinding | Description            |
| :--------- | :--------------------- |
| `C-c l a`  | Code actions           |
| `C-c l d`  | Find definitions       |
| `C-c l D`  | Find references        |
| `C-c l f`  | Format buffer          |
| `C-c l r`  | Rename symbol          |
| `C-c l i`  | Find implementation    |
| `C-c l t`  | Find type definition   |
| `M-.`      | Go to definition       |
| `M-,`      | Pop back from definition |

### Git (Magit & diff-hl)

| Keybinding | Description                         |
| :--------- | :---------------------------------- |
| `C-x g`    | Open Magit status                   |

`diff-hl` shows change indicators in the gutter for `prog-mode` and `dired-mode` buffers. Press `?` in Magit to see all commands.

### Completion (Vertico & Corfu)

**Vertico** (minibuffer completion):

- `C-n` or `Down` — Next completion
- `C-p` or `Up` — Previous completion
- `RET` — Select completion

**Corfu** (in-buffer completion):

- Triggers automatically after 2 characters
- `C-n` / `C-p` — Navigate completions
- `RET` — Accept completion
- `C-g` — Cancel

### Language REPLs

**Prefix**: `C-c f` opens the REPL command map

| Keybinding | Description                  |
| :--------- | :--------------------------- |
| `C-c f h`  | Haskell REPL (ghci)          |
| `C-c f o`  | OCaml REPL (utop)            |
| `C-c f e`  | Emacs Lisp REPL (ielm)       |

### Org Mode

| Keybinding | Description                |
| :--------- | :------------------------- |
| `C-c a`    | Org agenda                 |
| `C-c c`    | Org capture                |
| `C-c C-c`  | Execute babel source block |

### Org Citations (Citar, in org-mode)

| Keybinding | Description        |
| :--------- | :----------------- |
| `C-c b o`  | Open reference     |
| `C-c b i`  | Insert citation    |
| `C-c b n`  | Open citation notes |

### Org-noter (in org-mode)

| Keybinding | Description |
| :--------- | :---------- |
| `C-c N`    | Org-noter   |

### Org-roam (Zettelkasten Notes)

**Prefix**: `C-c n`

| Keybinding | Description              |
| :--------- | :----------------------- |
| `C-c n f`  | Find node                |
| `C-c n i`  | Insert node link         |
| `C-c n b`  | Toggle backlinks buffer  |
| `C-c n c`  | Capture to node          |
| `C-c n d`  | Daily note (today)       |

### Which-Key

- Press any prefix key (like `C-c`, `C-x`, or `C-c p`) and wait 0.3 seconds
- A popup will show all available completions for that prefix

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
| `C-a + [`        | Enter copy mode                                |
| `v`              | Begin selection (in copy mode)                 |
| `C-v`            | Rectangle selection (in copy mode)             |
| `y`              | Copy selection and exit (in copy mode)         |
| `C-a + I`        | Install plugins (TPM)                          |
| `C-a + U`        | Update plugins (TPM)                           |
| `C-a + C-s`      | Save session (resurrect)                       |
| `C-a + C-r`      | Restore session (resurrect)                    |
