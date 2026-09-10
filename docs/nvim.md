# Neovim Setup

**Plugin manager**: lazy.nvim
**Leader**: `<Space>` | **Local leader**: `,`
**Theme**: Modus Vivendi
**Config**: `~/.config/nvim/lua/naamanu/`

Tutorial guides:

- [Neovim Tutorials](nvim-tutorials.md)
- [Neovim Development Workflow](nvim-dev-workflow.md)
- [Neovim Note-Taking Workflow](nvim-note-taking-workflow.md)
- [Neovim Starting Projects](nvim-starting-projects.md)

---

## Structure

```text
init.lua                  vim.loader, leaders, require("naamanu.core")
stylua.toml               formatting rules for the tree (2 spaces, width 100)
lazy-lock.json            pinned plugin revisions (sync with `:Lazy sync`)
lsp/<server>.lua          one native vim.lsp.config table per server (17)
after/ftplugin/           c, cpp, rust: 4-wide indentation
queries/ocaml/            local rainbow-delimiters query
lua/naamanu/
  theme.lua               shared light/dark mode (theme-mode state file)
  core/
    init.lua              load order: options, keymaps, autocmds, lazy, lsp, workflows
    options.lua
    keymaps.lua           includes sessions (<leader><tab>s/o) and REPL keys
    autocmds.lua
    lazy.lua              lazy.nvim bootstrap; imports naamanu.plugins
    lsp.lua               capabilities, diagnostics, LspAttach, PythonEnv, server gate
    tasks.lua             project build/test commands (overseer) and the REPL
    workflows.lua         terminal agent and notes
  exact_plugins/          (applied as lua/naamanu/plugins/)
    cloak.lua
    colorscheme.lua
    completion.lua
    dap.lua
    editor.lua
    formatting.lua
    git.lua
    http.lua
    lectern.lua           local checkout guard
    linting.lua
    lsp.lua               fidget, lazydev, inc-rename, schemastore
    navigation.lua
    overseer.lua
    render-markdown.lua
    snacks.lua            snacks modules, <leader>u toggles
    treesitter.lua
    ui.lua                lualine, which-key groups, dropbar, rainbow, trouble
```

---

## Plugin Inventory

| Category | Plugin(s) |
| :--- | :--- |
| Completion | blink.cmp, friendly-snippets, native `vim.snippet` |
| LSP | native `vim.lsp`, shared PATH tools, fidget, schemastore, lazydev.nvim, inc-rename |
| Formatting | conform.nvim |
| Linting | nvim-lint |
| Git | gitsigns, diffview.nvim, snacks.lazygit |
| Navigation | snacks.explorer (sidebar), oil.nvim, snacks.picker (also backs `vim.ui.select`) |
| Treesitter | nvim-treesitter (main branch), nvim-treesitter-textobjects, nvim-ts-autotag, nvim-treesitter-context |
| Task runner | overseer.nvim |
| HTTP | kulala.nvim |
| Markdown | render-markdown.nvim, lectern.nvim (rendered side pane) |
| Tutor | lectern.nvim — interactive `claude` beside your work, driving the Claude skills |
| Editor | nvim-autopairs, nvim-surround, builtin `gc` comments, todo-comments, flash.nvim, undotree, treesj, snacks.zen |
| Agent | shared terminal agent (`<leader>a` group) |
| UI | snacks.nvim (indent, scope, scroll, statuscolumn, terminal), lualine, which-key, trouble.nvim, dropbar.nvim (breadcrumbs), rainbow-delimiters.nvim |
| Theme | modus-themes.nvim, `modus_vivendi` tinted variant — same as Emacs; Ghostty supplies Iosevka Comfy + Symbols Nerd Font |

Native LSP configuration (vim.lsp.config/enable) lives in `lua/naamanu/core/lsp.lua`, required from `core/init.lua` after lazy.nvim. The nvim-treesitter `main` branch needs the `tree-sitter` CLI on PATH to compile parsers (`brew install tree-sitter-cli`).

---

## Language Support

### LSP servers

| Language | Server |
| :--- | :--- |
| C/C++ | clangd |
| Rust | rust-analyzer |
| Python | ruff, basedpyright |
| Lua | lua-language-server |
| OCaml / Reason | ocaml-lsp |
| Haskell | haskell-language-server |
| Standard ML | millet |
| Racket | racket-langserver (`raco pkg install racket-langserver`) |
| TypeScript / JavaScript / React | vtsls, eslint-lsp |
| Vue / CSS | vtsls, css-lsp |
| JSON / YAML | json-lsp, yaml-language-server |

Ruff handles Python lint/fix actions. basedpyright owns Python hover and type intelligence (matching the Emacs setup); Ruff hover is disabled to avoid duplicate hover providers.

### Formatters

| Language(s) | Formatter |
| :--- | :--- |
| C, C++ | clang-format |
| Rust | rustfmt |
| Python | ruff fix + ruff format |
| OCaml | ocamlformat |
| Haskell | ormolu |
| Lua | stylua |
| Shell/Bash | shfmt |
| SQL | sql-formatter |
| JS, TS, JSX, TSX, Vue, JSON, YAML, Markdown, HTML, CSS, SCSS | prettier |

Format on save is enabled with a 3s timeout. JS/TS/Vue formatting uses Prettier directly and does not fall back to LSP formatting.

---

## Core Keybindings

### General

| Key | Action |
| :--- | :--- |
| `<Esc>` | Clear search highlights |
| `<C-h/j/k/l>` | Window navigation |
| `<C-Up/Down>` | Resize window height |
| `<C-Left/Right>` | Resize window width |
| `<C-s>` | Save file |
| `<leader>q` | Quit |
| `<leader>sv` / `<leader>sh` | Vertical / horizontal split |
| `<leader>bn` / `<leader>bp` / `<leader>bd` | Buffer next / previous / delete |

### Find and Navigation

| Key | Action |
| :--- | :--- |
| `<leader>e` | Toggle file sidebar (snacks.explorer) |
| `-` / `<leader>E` | Oil parent directory / Oil explorer |
| `<leader>'` / `<C-/>` | Toggle terminal on the right (also from inside it) |
| `<leader><tab>` `n x ] [ l r` | Tabs: new, close, next, prev, last, move; `gt`/`gT` |
| `<leader><tab>s` / `<leader><tab>o` | Save / open the session for the current directory |
| `<leader>;` | Pick a breadcrumb (dropbar) |
| `<leader>ff` | Find files |
| `<leader>fp` | Project files, using Git files when possible |
| `<leader>fr` | Recent files |
| `<leader>fg` | Live grep |
| `<leader>f/` | Search current buffer |
| `<leader>f.` | Resume last picker |
| `<leader>fc` | Grep word under cursor |
| `<leader>fb` | Buffers |
| `<leader>fh` | Help tags |
| `<leader>fk` | Keymaps |
| `<leader>fd` | Diagnostics |
| `<leader>fs` / `<leader>fS` | Document / workspace symbols |
| `<leader><leader>` | Flash jump |

### LSP

| Key | Action |
| :--- | :--- |
| `gd` / `gD` | Definition / declaration |
| `grr` / `gri` / `grt` | References / implementation / type definition (Neovim defaults) |
| `K` | Hover docs (Neovim default) |
| `<leader>la` | Code action |
| `<leader>lr` | Rename with live preview |
| `<leader>ld` | Line diagnostics |
| `<leader>ls` | Signature help |
| `<leader>ci` | Organize imports |
| `<leader>cI` | Add missing imports |
| `<leader>cu` | Remove unused imports |
| `<leader>cF` | Fix all auto-fixable issues |
| `<leader>cE` | Fix ESLint issues |
| `<leader>lh` | Toggle inlay hints |
| `<leader>lc` | Run code lens |
| `<leader>lC` | Enable code lens |
| `<leader>lR` | Restart attached LSP clients |

### Formatting and Linting

| Key | Action |
| :--- | :--- |
| `<leader>cf` | Format buffer or visual selection |
| `<leader>ll` | Trigger linting |

### Tasks - Overseer

| Key | Action |
| :--- | :--- |
| `<leader>or` | Run any Overseer task/template |
| `<leader>os` | Pick and run a `package.json` script |
| `<leader>od` | Run `dev` package script |
| `<leader>ol` | Run `lint` package script |
| `<leader>oy` | Run `typecheck` package script |
| `<leader>of` | Run `format` package script |
| `<leader>op` | Run current Python file |
| `<leader>oT` | Run Python tests with pytest |
| `<leader>ob` | Build current project |
| `<leader>on` | Run current project tests |
| `<leader>oC` | Compile current C/C++ file |
| `<leader>ot` | Toggle task panel |
| `<leader>oa` | Task action |

Project-aware tasks support Node package scripts, Python (`uv run python`, pytest), C/C++ (`cmake`, `meson`, `make`), Rust (`cargo`), OCaml (`dune`), and Haskell (`cabal`, `stack`).

### Git

| Key | Action |
| :--- | :--- |
| `]h` / `[h` | Next / previous hunk |
| `<leader>gs` / `<leader>gr` | Stage / reset hunk |
| `<leader>gS` / `<leader>gR` | Stage / reset buffer |
| `<leader>gu` | Undo stage hunk |
| `<leader>gp` / `<leader>gb` | Preview hunk / blame line |
| `<leader>gd` / `<leader>gD` | Diff this / diff against previous |
| `<leader>gg` | LazyGit |
| `<leader>go` / `<leader>gc` / `<leader>gh` | Diffview open / close / file history |

### Trouble

| Key | Action |
| :--- | :--- |
| `<leader>xx` | Workspace diagnostics |
| `<leader>xX` | Buffer diagnostics |
| `<leader>xQ` | Quickfix list |
| `<leader>cs` | Symbols |
| `<leader>cl` | LSP references/definitions |

### Treesitter

| Mode | Key | Action |
| :--- | :--- | :--- |
| n | `<C-space>` | Init / grow selection |
| n | `<BS>` | Shrink selection |
| x, o | `af` / `if` | Around / inside function |
| x, o | `ac` / `ic` | Around / inside class |
| n, x, o | `]f` / `[f` | Next / previous function |
| n, x, o | `]c` / `[c` | Next / previous class |

### Markdown pane — `<leader>m`

A read-only rendered-markdown float pinned to the right, so reference
material stays visible while you work in the main window. It is a `nofile`
buffer on purpose: render-markdown renders those in *every* mode, so the pane
keeps its formatting while you type next to it.

| Key | Action |
| :--- | :--- |
| `<leader>mm` | Toggle the pane |
| `<leader>mb` | Show the current buffer |
| `<leader>mf` | Pick a markdown file from the project |
| `<leader>mn` | Pick a note from `$NOTES_DIR` |
| `<leader>mF` | Focus the pane (`q` closes, `<Esc>` returns) |
| `<leader>mr` | Refresh from source |
| `<leader>md` / `<leader>mu` | Scroll the pane without leaving your window |

The pane also scrolls with the mouse while unfocused. It follows its source:
a buffer refreshes as you edit, a file on `:w` or when something outside
Neovim rewrites it. Plugin lives at `~/workspace/nvim/lectern.nvim`; the spec
is skipped when the checkout is absent, and `setup.sh` clones it on a fresh
machine.

### Tutor — `<leader>t`

An interactive `claude` session in the same right-hand column as the markdown
pane; open both and they stack, reference material above and the conversation
below. A real TUI rather than a headless one-shot, because several learning
skills depend on multi-turn structure — `guided-problem-solving` climbs a
five-rung hint ladder, `mastery-review` re-assesses after remediation — and a
one-shot invocation collapses them into handing over the answer.

| Key | Action |
| :--- | :--- |
| `<leader>tt` | Toggle the tutor |
| `<leader>ts` | Pick one of the skills in `~/.claude/skills` and start it |
| `<leader>tf` | Attach the current file as `@path` |
| `<leader>tv` | Attach the visual selection as `@path:from-to` |
| `<leader>td` | Attach the diagnostic under the cursor |
| `<leader>tp` | Attach whatever the markdown pane is showing |
| `<leader>tq` | Close the tutor |

Context is attached, never sent silently: each command echoes the reference
and types it into the prompt, and you press Enter. It calls `claude`
explicitly rather than `DEV_AGENT` — that probe prefers `codex`, which lacks
`learn-concept`, `paper-reading`, `proof-and-formalism` and
`research-question`.

### Editor Utilities

| Key | Action |
| :--- | :--- |
| `gcc`, visual `gc` / `gb` | Comment line / selection |
| `<leader>uu` | Toggle undotree |
| `<leader>z` | Toggle zen mode |
| `<leader>j` | Split / join node with treesj |
| `<leader>uh` / `<leader>ud` | Notification history / dismiss notifications |
| `<leader>rf` | Rename current file |
| `]r` / `[r` | Next / previous reference for word under cursor |
| `<leader>uw` `us` `ul` `ur` | Toggle wrap / spelling / line numbers / relative numbers |
| `<leader>ui` `ut` `uz` `uD` | Toggle inlay hints / treesitter highlight / zoom / diagnostics |
| `<leader>uv` / `<leader>uV` | Toggle diagnostic virtual text / virtual lines (current line) |

### REPL

Filetype picks the REPL, which runs in a snacks terminal on the right (Emacs: fp-repl):
OCaml `dune utop`/`utop`, Haskell `cabal repl`/`stack ghci`/`ghci`, Racket
`racket -il readline`, SML `sml`, Python `uv run ipython`/`ipython`. `<localleader>` is `,`.

| Key | Action |
| :--- | :--- |
| `<localleader>r` | Toggle the REPL for this buffer's language |
| `<localleader>e` | Send the current line, or the visual selection |
| `<localleader>b` | Send the whole buffer |

---

## Validation

Useful checks after config edits:

```sh
nvim --headless +qa
nvim --headless '+checkhealth vim.deprecated' '+qa'
stylua dot_config/nvim/lua/naamanu
```
