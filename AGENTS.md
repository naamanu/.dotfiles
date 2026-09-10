# AGENTS.md — chezmoi dotfiles source tree

This repository is the **chezmoi source**. File names map into `$HOME` by
chezmoi convention: `dot_emacs.d/` → `~/.emacs.d/`, `dot_config/fish/` →
`~/.config/fish/`, `dot_tmux.conf` → `~/.tmux.conf`. Every managed config
therefore exists twice — the source copy here and the applied copy in `$HOME`
— and the cardinal rule is to never leave the two out of sync.

## Layout

| Path | Maps to | What it is |
| :--- | :--- | :--- |
| `dot_emacs.d/` | `~/.emacs.d/` | Emacs 30+ config (macOS runs 31 via the `emacs-app` cask) — see "Emacs configuration" below |
| `dot_config/nvim/` | `~/.config/nvim/` | Neovim config — see "Neovim configuration" below |
| `dot_config/fish/` | `~/.config/fish/` | fish shell, completions, functions, `conf.d/` snippets |
| `dot_config/ghostty/`, `dot_config/aerospace/`, `dot_config/starship.toml`, `dot_tmux.conf` | terminal, window manager, prompt, tmux | |
| `dot_claude/` | `~/.claude/` | Claude Code user config, statusline, `skills/` |
| `dot_gitconfig`, `dot_zprofile` | git and login-shell config | |
| `private_dot_local/private_bin/` | `~/.local/bin/` | `dev-doctor`, plus `desktop-theme`/`desktop-wallpaper` — see "GNOME desktop" below |
| `setup.sh` | not applied | New-machine bootstrap: installs packages only, never writes config |
| `docs/`, `README.md`, `INSTALL.md` | not applied | Human documentation and keybinding reference |

Files listed in `.chezmoiignore` (docs, setup scripts, machine state like
`.emacs.d/var/` and `.claude/projects/`) are never applied to `$HOME`.

## Making changes — pick one direction per change

1. **Target-first** (preferred for anything you want to test live): edit the
   file in `$HOME`, verify it (see below), then `chezmoi add <target-path>`
   to import it back into this source tree.
2. **Source-first**: edit here, then `chezmoi apply <target-path>` with a
   targeted path (e.g. `chezmoi apply ~/.config/nvim`) so unrelated dirty
   files in `$HOME` are not touched.

Before and after, `chezmoi status` / `chezmoi diff` show drift. If a diff
shows changes you did not make, stop and surface them — another session or a
hand edit may be ahead of the source tree; do not blindly re-apply over it.

CAUTION: `dot_config/nvim/lua/naamanu/exact_plugins/` is an `exact_`
directory — `chezmoi apply` DELETES live files under
`~/.config/nvim/lua/naamanu/plugins/` that have no source counterpart. Never
apply that directory while a live-only spec exists un-added.

## Machine-local files — never add these to chezmoi

- `~/.emacs.d/custom.el` (per-machine `package-selected-packages`),
  `~/.emacs.d/local-pre.el`, `~/.emacs.d/local-post.el`
- `~/.emacs.d/elpa/`, `~/.emacs.d/var/`, `~/.emacs.d/tree-sitter/`, `~/.emacs.d/package-quickstart.el*`
- fish variables (`fish_variables`), caches, `.DS_Store`, Claude Code state

## Validation commands

| Tool | Command |
| :--- | :--- |
| Emacs | `emacs --batch -l ~/.emacs.d/init.el` — must exit 0 with no warnings |
| Emacs (interactive) | `C-c e h` (`M-x my/emacs-health-check`) for external tools |
| Neovim | `nvim --headless +qa`; deprecations: `nvim --headless '+checkhealth vim.deprecated' '+qa'` |
| Neovim (plugins) | `nvim --headless "+Lazy! sync" +qa` after spec changes |
| fish | `fish -n <file>` |
| tmux | `tmux -L test -f ~/.tmux.conf start-server` |
| shell scripts | `bash -n setup.sh`; `shellcheck` when available |
| ghostty | `ghostty +validate-config` — must exit 0 |
| desktop | `desktop-theme --dry-run` (writes nothing); `desktop-theme --check` |

## Conventions

- Keep edits small and grouped by tool; one tool per commit where possible.
- Commit subjects are short and imperative, optionally with a conventional
  prefix: `fix: restore python lsp config`, `simplify tmux theme`.
- Lua is formatted with Stylua. Shell should be explicit and side-effect free.
- Adding an external tool dependency? Follow the `dotfiles-add-tool` skill:
  it must land in `setup.sh`, the Emacs health check (if Emacs uses it), and
  `INSTALL.md` together.
- Python tooling is uv-first: project environments via `uv`/`.envrc` (envrc +
  direnv), per-project dev deps like debugpy via `uv add --dev`.
- **Both editors use the same LSP server per language** (basedpyright, vtsls,
  rust-analyzer, ocamllsp, yaml-language-server) so diagnostics agree.

---

## Emacs configuration (`dot_emacs.d/`)

Emacs 30+ (31 on macOS), `use-package` with `use-package-always-ensure t` (built-in packages
get `:ensure nil`), completion via vertico/orderless/marginalia/consult +
corfu/cape/embark, LSP via eglot, tree-sitter major modes throughout.

### Module load order (init.el)

`elisp/` modules load in dependency order; **keys.el must stay last** so every
keymap it binds into already exists:

1. `core.el` — editor defaults, repeat-mode, Dired, theme, fontaine/pulsar, `my/emacs-health-check`
2. `completion.el` — minibuffer and in-buffer completion stack
3. `dev.el` — project.el helpers, eglot, apheleia, magit/forge, envrc, vterm, dape
4. `langs.el` — tree-sitter grammars/remaps and per-language hooks
5. `notes.el` — org, denote, citar, cdlatex/org-fragtog, olivetti, jinx, org-present
6. `vim.el` — evil, evil-collection, evil-surround, evil-commentary, evil-org
   (file is `vim.el`, not `evil.el`, so it cannot shadow the evil package)
7. `keys.el` — every global binding, including the evil `SPC` leader;
   prefixes documented in its Commentary

`local-pre.el` / `local-post.el` (machine-local, not in chezmoi) load around
everything; `custom.el` holds only `package-selected-packages`.

### Rules

- **New package** → `use-package` form in the right module AND the package
  name added to `package-selected-packages` in `~/.emacs.d/custom.el`
  (machine-local; keeps `package-autoremove` safe).
- **New external binary** → row in `my/emacs-health-checks` (core.el) and in
  `setup.sh`. Exception: Python modules (e.g. debugpy) are per-project via
  `uv add --dev`, not health-checked globally.
- **New language** → grammar in `treesit-language-source-alist`, mode remap or
  `auto-mode-alist` entry, a `my/<lang>-mode-defaults` hook setting
  `compile-command` + `my/eglot-ensure-when-executable`, and entries in
  `eglot-server-programs` (dev.el) and `apheleia-mode-alist` if applicable.
- Keybindings live in `keys.el` only, on the established `C-c` prefix maps.
  The evil leader (`SPC`) mirrors those maps; a new `C-c x` prefix map also
  gets a `<leader> x` line in the Leader section (end of keys.el).
- Never add `flymake` to `eglot-stay-out-of` (regexp-matched; silently kills
  all LSP diagnostics — see the NOTE in dev.el).
- Home-grown Neovim plugins load from `~/workspace/nvim/` by lazy `dir =`
  (e.g. `exact_plugins/lectern.lua`). The spec file returns `{}` when the
  checkout is absent, so a fresh machine still starts — the Lua equivalent of
  the Emacs `:if (file-directory-p ...)` guard. Repo: naamanu/lectern.nvim,
  cloned by `setup.sh` alongside the `~/workspace/elisp/` packages.
- Three home-grown packages load from `~/workspace/elisp/` by `:load-path`
  (langs.el): `fp-repl`, `dune-transient` and `mli-lens`.  Each `use-package`
  form is guarded with `:if (file-directory-p ...)`, so a machine without
  those checkouts still starts cleanly.  They are NOT chezmoi-managed; their
  repos are naamanu/fp-repl, naamanu/dune-transient and naamanu/mli-lens, and
  `setup.sh` clones all three (with lectern.nvim) in the post-install section.
- `utop-eros` (naamanu/utop-eros) is superseded by `fp-repl`, which covers
  OCaml, Haskell, Racket and SML behind one set of keys.  Do NOT wire both:
  they advise the same utop functions twice.
- `evil` is `:pin melpa` (vim.el): the ELPA 1.15.0 release breaks on Emacs 31
  (`void-variable evil-mode-buffers` in post-command-hook). Keep the pin
  until a release newer than 1.15.0 lands on NonGNU ELPA.
- `dirvish` is `:pin melpa` (core.el): NonGNU's tarball keeps its extensions
  (`dirvish-side`, `dirvish-vc`, ...) in a subdirectory package.el does not
  add to `load-path`; MELPA flattens them.

After verifying: `chezmoi add ~/.emacs.d/init.el ~/.emacs.d/early-init.el
~/.emacs.d/elisp/*.el`.

---

## Neovim configuration (`dot_config/nvim/`)

Neovim 0.12+, lazy.nvim, ~36 plugins. `stylua.toml` (2 spaces, width 100)
formats the tree: run `stylua ~/.config/nvim` before `chezmoi add`.

### Load order

`init.lua` (`vim.loader.enable()`, leaders) → `lua/naamanu/core/init.lua`:
options → keymaps → autocmds → lazy → **lsp** (after lazy so lazy-loaded
plugin modules resolve from server callbacks) → workflows. Plugin specs are
auto-imported from `lua/naamanu/plugins/*.lua`, one file per concern.
`core/tasks.lua` is the bespoke project build/test command layer consumed by
the overseer keymaps, and holds the REPL (`<localleader>r/e/b`).
Per-server LSP tables live in `lsp/<name>.lua` at the config root (the native
runtimepath `lsp/` directory, `:h lsp-config`); `core/lsp.lua` keeps only what
is shared. `after/ftplugin/<ft>.lua` holds per-filetype settings.

### Non-obvious constraints

- **LSP is the native API** (`vim.lsp.config`/`vim.lsp.enable`; tables in
  `lsp/<name>.lua`, shared parts in `core/lsp.lua`; no nvim-lspconfig, no
  mason). nvim-lspconfig concepts like `on_new_config` and
  `single_file_support` DO NOT EXIST here and fail silently — do not
  reintroduce them. Servers are enabled by the gate table at the bottom of
  `core/lsp.lua` only when their binary is on PATH (racket-langserver is
  gated on the raco package directory), so missing tools degrade silently.
  `vim.lsp.enable` loads every enabled `lsp/<name>.lua` at startup, so keep
  plugin requires (schemastore) inside `on_init`, as jsonls/yamlls do.
- **blink capabilities are merged in `before_init`** on the `*` config, not
  by an eager `require("blink.cmp")` at startup. A server file only sets
  `capabilities` for something else (ruff pins `positionEncodings` to utf-16
  so it matches basedpyright); completion support is already covered.
- **Python interpreter** for basedpyright is resolved per client in
  `lsp/basedpyright.lua`'s `on_init` from `core/lsp.lua`'s `python_path`
  (deep-copies `client.settings`, then re-sends didChangeConfiguration).
  `:PythonEnv` reports the result. Keep that block intact; it is why imports
  resolve when Neovim is started outside the project root.
- **nvim-treesitter is the `main` rewrite branch** (pinned). Its `setup()`
  only accepts `install_dir`; master-branch option tables
  (`ensure_installed`/`highlight`/`indent`/`incremental_selection`) are
  silently ignored. Parsers install via `ts.install()` in
  `plugins/treesitter.lua` and need the `tree-sitter` CLI on PATH.
  Highlighting starts per buffer via a FileType autocmd calling
  `vim.treesitter.start`.
- **Python is basedpyright + ruff** (ruff owns imports/formatting, hover
  disabled). Do not add `ty` or a second Python diagnostics source (e.g.
  ruff in nvim-lint) — it doubles every message.
- **Prefer a snacks module over a new plugin** (picker, ui_select, zen,
  lazygit, notifier, scratch are in use). Exception: fidget stays — snacks
  has no LSP progress display.
- Completion is blink.cmp with native `vim.snippet` — no LuaSnip, no
  nvim-cmp. Commenting is builtin `gc` — no Comment.nvim.
- which-key groups are declared in `plugins/ui.lua`; never bind a bare key
  that is also a group prefix (hence `<leader>uu` undotree, `<leader>nh`
  notifier history).

### Adding things

- **Plugin** → new/existing file in `lua/naamanu/plugins/`, then
  `nvim --headless "+Lazy! sync" +qa`; sync the spec file AND
  `lazy-lock.json` to chezmoi.
- **LSP server** → new `lsp/<name>.lua` returning the `vim.lsp.config`
  table (`cmd`, `filetypes`, `root_markers`, `settings`), a `{ name, binary }`
  row in the `servers` gate table in `core/lsp.lua`, and the binary installed
  by `setup.sh` (see the `dotfiles-add-tool` skill). Nothing installs servers
  from inside Neovim.
- **Language** → parser in `ensure_installed` (plugins/treesitter.lua),
  formatter in `plugins/formatting.lua` (conform), server as above, test
  commands in `core/tasks.lua` if overseer should know it, REPL command in
  `repl_command` (core/tasks.lua), indentation in `after/ftplugin/<ft>.lua`.

---

## GNOME desktop (`private_dot_local/private_bin/`)

Linux only. A macOS-inspired GNOME desktop that keeps the **stock shell
theme** — no WhiteSur or other third-party GTK theme, because those break on
every GNOME release. The look comes from extensions, icons, cursors, fonts
and spacing instead. Full reference: `docs/desktop.md`.

The same split as the rest of the repo: `setup.sh` installs the parts and
writes no config; `desktop-theme` writes every gsettings key.

### Rules

- **All settings live in one table** — the `settings()` heredoc in
  `desktop-theme`, one `uuid|schema|key|value` record per line. `--reset`
  walks the same table, so it resets exactly what was set and leaves
  neighbouring keys in those schemas alone. Never scatter `gsettings set`
  calls through the script.
- **Never trust a guessed schema key.** Read the real ones first:
  `gsettings --schemadir ~/.local/share/gnome-shell/extensions/<uuid>/schemas
  list-recursively <schema>`. Extension schemas are not on the default
  search path, which is why every extension record carries its UUID.
- **Enable extensions by writing `org.gnome.shell enabled-extensions`**, not
  via `gnome-extensions enable`. That command talks to the running shell,
  which only knows about extensions it has already scanned — on the run that
  installs them it reports "not installed" and silently does nothing.
- **New extension** → UUID in `ENABLE_EXTENSIONS` (desktop-theme), an
  `install_gnome_extension` line in `setup.sh`, and a row in the table in
  `docs/desktop.md`.
- **Keybindings are deliberately untouched**, so `Ctrl+C`/`Ctrl+V` keep
  working in terminals and Emacs. `Super+Space` is the sole exception
  (GNOME's input-source switcher, useless with one layout, reused for the
  Spotlight launcher). Do not add more without asking.
- Wallpapers are **generated, never committed** — `desktop-wallpaper` renders
  them from colour control points via ImageMagick. Adding a palette means a
  new line in its `palettes()` table, nothing else.

### Validation

`desktop-theme --dry-run` writes nothing and prints every setting;
`desktop-theme --check` reports which parts are installed. Both scripts must
stay `shellcheck`-clean. After applying, extensions load only at shell
startup — log out and back in, or `Alt+F2` then `r` on X11.
