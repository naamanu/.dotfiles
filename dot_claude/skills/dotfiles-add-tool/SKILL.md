---
name: dotfiles-add-tool
description: Use when adding a new external tool to the dotfiles — a CLI, language server, formatter, debugger, font, or Emacs/Neovim package — so it lands everywhere it needs to (setup.sh, INSTALL.md, editor config) instead of only on the current machine.
---

# Adding a tool to the dotfiles

A tool added only on this machine silently breaks the next bootstrap. Touch
every checkpoint that applies, in the chezmoi source tree, then sync.

## Checklist

1. **Install it here** the same way `setup.sh` would (brew formula, `uv tool
   install`, etc.) and verify it works.
2. **`setup.sh`** — add it to the right section (core / CLI / languages /
   LSP-formatters / macOS-only). Match the existing `install_pkg` /
   `brew_install` / `install_optional_pkg` idiom, and handle both platforms
   or use the platform-specific branch. `bash -n setup.sh` after.
3. **Emacs** — Emacs is a single, deliberately small `init.el` (Haskell,
   OCaml, notes only). There is no health-check table any more; only wire a
   tool into Emacs if it serves those three.
4. **Editor wiring** — as applicable:
   - Emacs: a `use-package` form in `init.el` + the name in
     `package-selected-packages` in machine-local `~/.emacs.d/custom.el`
   - Neovim LSP: `lsp.config(...)` + `mason_servers` entry in `core/lsp.lua`,
     mason package in `ensure_installed` (plugins/lsp.lua); the two editors
     should use the SAME server per language (e.g. basedpyright, vtsls)
   - Neovim formatter: `core/format.lua` (only if the language server does
     not format)
   - Neovim language: parser in `parsers` (core/plugins.lua)
   - Neovim plugin: add to `vim.pack.add` in `core/plugins.lua` (only if
     0.12 has no built-in for it), restart, sync `nvim-pack-lock.json` too
5. **`INSTALL.md`** — add it to the "What Gets Installed" list.
6. **Verify**: `emacs --batch -l ~/.emacs.d/init.el` exits 0.
7. **Sync**: `chezmoi add` any `$HOME` files you edited; confirm with
   `chezmoi status`. Commit one tool per commit when asked.

## Removal is the same list in reverse

Also remove the package from `package-selected-packages` so
`package-autoremove` can collect it, and delete any keybindings in `keys.el`.
