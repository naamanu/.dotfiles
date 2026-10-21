---
name: dotfiles-sync
description: Use when changing any chezmoi-managed dotfile on this machine — Emacs, Neovim, fish, tmux, ghostty, aerospace, starship, git, or Claude Code config. Covers the edit → verify → chezmoi sync loop, per-tool validation commands, and what must never be imported into the source tree. Also use when `chezmoi status`/`diff` shows unexplained drift.
---

# Syncing chezmoi-managed dotfiles

The chezmoi source tree lives at `$(chezmoi source-path)` (usually
`~/.local/share/chezmoi`). Every managed file exists twice: source copy there,
applied copy in `$HOME`. A change is not done until both match.

## Workflow

1. **Check for pre-existing drift first**: `chezmoi status`. If files you are
   NOT about to touch show as modified, report that to the user before
   proceeding — do not fold someone else's drift into your change.
2. **Edit the live target in `$HOME`** (preferred — it can be tested
   immediately). Editing source-first is fine too; then apply with a targeted
   path only: `chezmoi apply ~/.config/nvim`, never a bare `chezmoi apply`.
3. **Verify** with the matching command:
   - Emacs: `emacs --batch -l ~/.emacs.d/init.el` → exit 0, no warnings
   - Neovim: `nvim --headless +qa`
   - fish: `fish -n <changed file>`
   - tmux: `tmux -L test -f ~/.tmux.conf start-server`
   - shell scripts: `bash -n <script>`, plus `shellcheck` if available
4. **Sync back**: `chezmoi add <target-path>` for each changed file.
5. **Confirm clean**: `chezmoi status` shows nothing for the touched paths.
6. **Commit** in the source repo only when asked: short imperative subject,
   one tool per commit (`fix: restore python lsp config`).

## Never import into chezmoi

`~/.emacs.d/custom.el`, `local-pre.el`, `local-post.el`, `elpa/`, `var/`,
`tree-sitter/`; `fish_variables`; caches; `.DS_Store`; `~/.claude` state
(projects, history, shell-snapshots — see `.chezmoiignore`). If `chezmoi add`
is about to pick one of these up, stop.

## Editor-specific rules

The root `AGENTS.md` in the chezmoi source repo has a section per editor —
read the relevant one before editing:

- **Emacs**: one small `init.el` (stock look, evil, Haskell/OCaml, notes).
  New packages also go in `package-selected-packages` in machine-local
  `custom.el`.
- **Neovim**: native LSP API constraints, treesitter main-branch semantics,
  snacks conventions. After any plugin-spec change run
  `nvim --headless "+Lazy! sync" +qa` and `chezmoi add` the spec file AND
  `lazy-lock.json`. CAUTION: the plugins dir is `exact_plugins/` in source —
  `chezmoi apply` DELETES live plugin files that have no source counterpart,
  so never apply that directory while a live-only spec exists un-added.
