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
| `private_dot_local/private_bin/` | `~/.local/bin/` | `dev-doctor`, `keys`, `menu`, `theme-mode` (shared light/dark switch), plus `desktop-theme`/`desktop-wallpaper` — see "GNOME desktop" below |
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


## Machine-local files — never add these to chezmoi

- `~/.emacs.d/custom.el` (per-machine `package-selected-packages`),
  `~/.emacs.d/local-pre.el`, `~/.emacs.d/local-post.el`
- `~/.emacs.d/elpa/`, `~/.emacs.d/var/`, `~/.emacs.d/tree-sitter/`, `~/.emacs.d/package-quickstart.el*`
- fish variables (`fish_variables`), caches, `.DS_Store`, Claude Code state

## Validation commands

| Tool | Command |
| :--- | :--- |
| Emacs | `emacs --batch -l ~/.emacs.d/init.el` — must exit 0 with no warnings (`--batch` implies `-q`; init.el loads early-init.el and activates packages itself in that case) |
| Emacs (daemon) | `emacs --daemon=check && emacsclient -s check -e '(featurep (quote evil))' && emacsclient -s check -e '(kill-emacs)'` |
| Neovim | `nvim --headless +qa`; deprecations: `nvim --headless '+checkhealth vim.deprecated' '+qa'` |
| Neovim (plugins) | first start installs from `vim.pack.add`; updates: `:lua vim.pack.update()`, review, `:write`; then `chezmoi add ~/.config/nvim/nvim-pack-lock.json` |
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
  it must land in `setup.sh` and `INSTALL.md` together.
- Python tooling is uv-first: project environments via `uv`/`.envrc` (envrc +
  direnv), per-project dev deps like debugpy via `uv add --dev`.
- **Both editors use the same LSP server per language** (basedpyright, vtsls,
  rust-analyzer, ocamllsp, yaml-language-server) so diagnostics agree.

---

## Emacs configuration (`dot_emacs.d/`)

Deliberately close to stock Emacs 31 (the `emacs-app` cask on macOS): the
default theme, mode line and keys. **One file, `init.el`** (~330 lines), plus
`early-init.el` (package archives, quickstart, frame chrome). It adds only:

- evil (+ evil-collection/surround/commentary/org) and a small `SPC` leader;
  vertico/orderless/marginalia/consult; built-in `completion-preview-mode`.
- Python: eglot + basedpyright, ruff format on save. C/C++: eglot + clangd.
- Haskell: haskell-mode + interactive-haskell-mode, eglot + hls.
- OCaml: tuareg, eglot + ocamllsp, ocaml-eglot, utop/dune (from the opam
  switch's site-lisp), and the home-grown `fp-repl`, `dune-transient`,
  `ocaml-spec-kit` (mli-lens) from `~/workspace/elisp/`.
- Notes: Denote (markdown-yaml in `~/notes`, byte-compatible with Neovim's
  `core/notes.lua`), denote-journal/markdown, consult-denote; Org agenda +
  capture over `~/org`. Keys: `C-c n` notes, `C-c a` agenda, `C-c c` capture.

Eglot formats on save when the server can (ormolu via hls, ocamlformat via
ocamllsp, clang-format via clangd); Python uses ruff directly. Font: stock
Menlo at 16pt (`set-face-attribute 'default`). Theme: monokai-theme, dark in
both modes; `my/apply-theme-mode` is a no-op and `theme-mode --check` reports
it as fixed. monokai-theme 3.5.3 boxes the mode line with `:style
unspecified`, which Emacs 31 rejects in GUI frames: init.el loads the theme
without enabling it, strips that pair (`my/drop-unspecified-box-style`), then
enables it. Test themes in a GUI frame — batch/daemon checks miss face errors.

The previous modular config (core/completion/dev/langs/notes/vim/keys.el) is
in git history before this change and in
`~/.local/share/emacs-config-backup-2026-10-21/` on this machine.

### Rules

- **New package** → a `use-package` form in `init.el` AND the name in
  `package-selected-packages` in `~/.emacs.d/custom.el` (machine-local; keeps
  `package-autoremove` safe).
- Keep it small: anything stock Emacs already does well stays stock.
- `evil` is `:pin melpa`: the ELPA 1.15.0 release breaks on Emacs 31
  (`void-variable evil-mode-buffers`). Keep the pin until a newer release
  lands on NonGNU ELPA.
- Home-grown packages load from `~/workspace/elisp/` by `:load-path`, each
  guarded with `:if (file-directory-p ...)`, so a machine without the
  checkouts still starts. They are NOT chezmoi-managed; `setup.sh` clones them.

After verifying: `chezmoi add ~/.emacs.d/init.el ~/.emacs.d/early-init.el`.

---

## Neovim configuration (`dot_config/nvim/`)

Deliberately near-stock Neovim 0.12. Tooling covers **Python, TS/JS, C/C++,
SQL, OCaml and Haskell** only. `stylua.toml` (2 spaces, width 100) formats
the tree: run `stylua lua lsp after` from `~/.config/nvim` (stylua only finds
the config from the cwd) before `chezmoi add`.

### Layout and load order

`init.lua` (`vim.loader.enable()`, leaders) → `lua/naamanu/core/init.lua`:
options → keymaps → autocmds → **plugins** → theme → lsp → format → workflows.

- `core/plugins.lua` — the ONLY plugin list: one `vim.pack.add` (built-in
  package manager; revisions in `nvim-pack-lock.json`) with each plugin's
  setup and keys: nvim-treesitter (`main` branch, parsers for the languages
  above), oil.nvim, snacks.nvim (picker + terminal only), gitsigns.nvim,
  vim-tmux-navigator, and one theme, monokai.nvim (matching Ghostty's Monokai
  Classic and Emacs). Plus the built-in `nvim.undotree` package.
- `core/lsp.lua` — native `vim.lsp.config`/`enable`; servers in
  `lsp/<name>.lua` (basedpyright, ruff, vtsls, eslint, clangd, ocamllsp,
  hls), enabled only when the binary is on PATH. Completion is built in:
  `vim.lsp.completion` (autotrigger) + `'autocomplete'` with `complete=o,.`.
- `core/format.lua` — format on save without conform: project prettier for
  TS/JS/JSON, sql-formatter for SQL, LSP formatting otherwise.
- `core/tasks.lua` — build/test/watch commands (run in a snacks terminal)
  and the REPL (`<localleader>r/e/b/f`).
- `theme.lua` — shared light/dark mode from theme-mode(1) plus a family
  picker (`<leader>uC`) over Neovim's built-in colorschemes and the four
  monokai.nvim variants (all dark-only; pin dark to keep one in light mode).

### Rules

- **Prefer built-ins.** A plugin is added only when 0.12 cannot do the job
  (completion, snippets, comments, undotree, cmdline UI, LSP are built in).
- **LSP is the native API** — no nvim-lspconfig, no mason. nvim-lspconfig
  concepts like `on_new_config`/`single_file_support` silently do nothing.
  `vim.lsp.enable` loads every enabled `lsp/<name>.lua` at startup.
- **nvim-treesitter is the `main` branch**: `setup()` takes only
  `install_dir`; parsers install via `install()`; highlighting starts per
  buffer via `vim.treesitter.start`. Needs the `tree-sitter` CLI.
- **Python is basedpyright + ruff** (ruff formats; basedpyright never does).
- hls must be started with `--lsp` (`lsp/hls.lua`); without it the wrapper
  runs its setup tester and exits.

### Adding things

- **Plugin** → add it to `vim.pack.add` in `core/plugins.lua` with its setup
  in the same file, restart, then `chezmoi add` `core/plugins.lua` AND
  `nvim-pack-lock.json`. Removing one: delete it from the list, then
  `:lua vim.pack.del({ "name" })`.
- **LSP server** → `lsp/<name>.lua`, a `{ name, binary }` row in the
  `servers` table in `core/lsp.lua`, and the binary in `setup.sh`.
- **Language** → parser in `parsers` (core/plugins.lua), server as above,
  formatter in `core/format.lua` if the server does not format, build/test
  commands and REPL in `core/tasks.lua`.

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
