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

Deliberately near-stock Neovim 0.12. Tooling covers **Python, TS/JS, Go,
Rust, C/C++, Java, Lua, SQL, OCaml and Haskell**, plus shell, JSON, YAML,
TOML and Markdown. `stylua.toml` (2 spaces, width 100) formats
the tree: run `stylua lua lsp after` from `~/.config/nvim` (stylua only finds
the config from the cwd) before `chezmoi add`.

### Layout and load order

`init.lua` (`vim.loader.enable()`, leaders) → `lua/naamanu/core/init.lua`:
options → keymaps → autocmds → **plugins** → ui → git → theme → tabline →
lsp → format → dap → workflows. Each module loads in its own `pcall`, so one failure cannot
stop the rest; failures are reported once after startup. Startup must stay
error-free: check `nvim --headless +qa` AND a real UI (ui2, tiny-cmdline and
the tabline only run with a UI attached), e.g. nvim in a detached tmux pane,
then `:messages` and `v:errmsg`.

- `core/plugins.lua` — the ONLY plugin list: one `vim.pack.add` (built-in
  package manager; revisions in `nvim-pack-lock.json`), plus mason +
  mason-tool-installer, mini.pairs, mini.ai (treesitter text objects
  f/c/o/u from nvim-treesitter-textobjects' queries, plus a/brackets/
  quotes), mini.surround (on `gs`: gsa/gsd/gsr/gsf/gsF/gsh, so `s` stays
  stock), blink.cmp, and nvim-treesitter (`main` branch; ~40 parsers listed
  in `parsers`, missing ones installed just after startup; a FileType
  autocmd starts highlighting, folds, treesitter indent where an indents
  query exists, and buffer-local `]m [m ]M [M ]k [k` moves -- scheduled so
  they win over ftplugin maps; `:TSUpdate` runs on PackChanged). Plus the
  built-in `nvim.undotree` package.
- `core/ui.lua` — mini.icons (also mocks nvim-web-devicons), tiny-cmdline
  (centered ui2 cmdline), lualine (one global statusline; project root =
  nearest .git/lua/package.json/Makefile/Cargo.toml/go.mod/pyproject.toml
  via `vim.fs.root`), incline (file tag top-right of each window, hidden on
  the cursor's line), todo-comments (+ plenary), nvim-highlight-colors
  (hex/rgb/hsl/ANSI/CSS var as background; named colors and Tailwind off),
  oil (always a centered float with a live preview -- `-`, `nvim .`,
  `:e dir` and `:Oil` all end up there, `q` closes; replaces netrw; renames
  update imports through the LSP and save the edited buffers -- a
  `workspace/applyEdit` wrapper covers servers like vtsls that only
  implement didRenameFiles), snacks (picker, terminal, notifier, input,
  bigfile, indent, scope with `ii`/`ai` and `[i`/`]i`, statuscolumn, words
  with `]]`/`[[`; scroll off). snacks' input/scope/picker start on UIEnter,
  so headless tests must `doautocmd UIEnter` to see their keys.
- `core/git.lua` — gitsigns and codediff. codediff is configured through
  `require("codediff.config").setup` (not `codediff.setup`, which loads the
  whole diff UI at startup).
- No theme plugin: `colors/monokai-pro.lua` is built from Ghostty's "Monokai
  Pro" palette (treesitter + LSP semantic groups included); keep the two in
  step if either changes.
- `core/lsp.lua` — native `vim.lsp.config`/`enable` over nvim-lspconfig's
  defaults; our overrides in `after/lsp/<name>.lua`. Every server in the
  `vim.lsp.enable` list is on (Neovim skips one whose binary is missing);
  `vim.lsp.config("*")` gives all of them file-rename capabilities
  (willRename/didRename). Binaries come from mason, except clangd (Xcode),
  ocamllsp (opam), hls (ghcup/brew) and jdtls (brew), which must match their
  toolchain. jdtls needs JDK 25+ to run: `after/lsp/jdtls.lua` sets
  `cmd_env.JAVA_HOME` to brew's newest openjdk and registers `$JAVA_HOME`
  (21) as the project runtime -- mason's jdtls fails on JDK 21 (exit 13).
  The same `"*"` config adds blink.cmp's completion capabilities.
  LspAttach turns server document colors off (nvim-highlight-colors paints
  them) and enables `vim.lsp.inline_completion` for servers that offer it
  (copilot; `<leader>uA` toggles, `<M-]>`/`<M-[>` cycle). `<leader>lR` is
  `:lsp restart`. jsonls/yamlls take schemas from SchemaStore.nvim in
  `before_init`.
- Completion is blink.cmp (`core/plugins.lua`, pinned `^1`): `enter`
  preset (<CR> accepts; <Tab> accepts an inline suggestion first, then
  jumps snippet fields), auto-brackets, docs after 200ms, no ghost text,
  treesitter-highlighted menu with mini.icons kind icons, and the menu
  auto-opens in the cmdline. `vim.lsp.completion`/`'autocomplete'` are off.
- Pairs are mini.pairs (`core/plugins.lua`), in insert and command-line
  mode.
- `core/format.lua` — conform.nvim, format on save with a 1000ms timeout
  and LSP fallback: web files (js/ts/jsx/tsx/json/css/scss/less/html/
  markdown/mdx/yaml) take the first installed of oxfmt, prettierd,
  prettier; stylua for Lua, shfmt for sh/bash, fish_indent for fish,
  sql-formatter for SQL; Go runs gopls organize-imports first.
  `:ToggleFormatter` turns format on save off for the current buffer.
- `core/dap.lua` — nvim-dap, set up on the first debug key (not at
  startup): Go through `dlv dap`, Python through nvim-dap-python with
  mason's `debugpy-adapter`, running the program under the interpreter
  `core/lsp.lua`'s `python_path()` finds; `<leader>dt` debugs the test at
  the cursor (Go or pytest).
- `core/options.lua` — also routes ui2 messages. The search-count and
  typed-command tweaks wrap the private `vim._core.ui2.messages.msg_show`;
  if a Neovim release moves it they are skipped with one warning. Retest
  messages after every Neovim upgrade. Remote-plugin providers (python3,
  node, perl, ruby) are disabled; nothing needs them.
- `core/tasks.lua` — build/test/watch commands (run in a snacks terminal)
  and the REPL (`<localleader>r/e/b/f`).
- `core/tabline.lua` — custom tabline, only with 2+ tabs: icon + file name of
  each tab's active window between slanted separators, names cut at 20
  chars; an oil tab shows its directory (`~/foo`, no icon), cut from the left. Colors are the `TabLine*` groups in `colors/monokai-pro.lua`.
- `theme.lua` — shared light/dark mode from theme-mode(1) plus a family
  picker (`<leader>uC`) over Neovim's built-in colorschemes and `monokai`
  (the default; monokai-pro in both modes, like Ghostty).

### Rules

- **Prefer built-ins.** A plugin is added only when 0.12 cannot do the job
  (snippets, comments, undotree, cmdline UI, LSP, inline completion,
  `:lsp restart`, `:restart`/`ZR` are built in; completion (blink.cmp) and
  formatting (conform.nvim) are the exceptions).
- **LSP is the native API with nvim-lspconfig as data only** — its
  `lsp/<name>.lua` files supply defaults; never the legacy
  `require("lspconfig").x.setup()`. Overrides go in `after/lsp/` (higher
  merge priority than `lsp/`; `:h lsp-config-merge`). A config whose
  lspconfig default has a `root_dir` function ignores our `root_markers`.
- **mason installs, never updates**: `mason-tool-installer` with
  `run_on_start = true`, `auto_update = false`. Update deliberately with
  `:MasonToolsUpdate`.
- **nvim-treesitter is the `main` branch**: `setup()` takes only
  `install_dir`; parsers install via `install()`; highlighting starts per
  buffer via `vim.treesitter.start`. Needs the `tree-sitter` CLI.
- **Python is basedpyright + ruff** (ruff formats; basedpyright never does).
- hls must be started with `--lsp` (lspconfig's default does); without it
  the wrapper runs its setup tester and exits.

### Adding things

- **Plugin** → add it to `vim.pack.add` in `core/plugins.lua`, its setup in
  the module for its topic (plugins/ui/git/format/dap), restart, then
  `chezmoi add` the changed modules AND `nvim-pack-lock.json`. Removing one: delete it from the list, then
  `:lua vim.pack.del({ "name" })`.
- **LSP server** → its lspconfig name in the `vim.lsp.enable` list
  (`core/lsp.lua`), the mason package in `ensure_installed`
  (`core/plugins.lua`), and `after/lsp/<name>.lua` only for real overrides.
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
