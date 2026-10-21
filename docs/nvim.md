# Neovim Setup

Near-stock Neovim 0.12: the stock look and keys, built-in LSP, completion,
snippets, comments and undotree, and five plugins for what Neovim cannot do
itself. Tooling covers **Python, TypeScript/JavaScript, C/C++, SQL, OCaml and
Haskell**.

## Structure

```text
init.lua                  vim.loader, leaders (<Space>, local leader ",")
nvim-pack-lock.json       plugin revisions (vim.pack)
lsp/<server>.lua          basedpyright, ruff, vtsls, eslint, clangd, ocamllsp, hls
after/ftplugin/           per-filetype indentation and OCaml LSP extras
lua/naamanu/
  theme.lua               shared light/dark mode + built-in colorscheme picker
  core/
    init.lua              load order
    options.lua           options, completion, :grep via rg, ui2
    keymaps.lua           general keys, sessions, REPL, build/test keys
    autocmds.lua
    plugins.lua           the only plugin list (vim.pack) and its setup
    lsp.lua               diagnostics, LspAttach keys, completion, server gate
    format.lua            format on save (prettier / sql-formatter / LSP)
    tasks.lua             project build/test/watch commands and the REPL
    workflows.lua         terminal agent, docs lookup
```

## Plugins

| Plugin | Why |
| :--- | :--- |
| nvim-treesitter (`main`) | parsers for python, ts/tsx/js, json, sql, ocaml, haskell, c/cpp |
| oil.nvim | directories as editable buffers (`-`) |
| snacks.nvim | picker (files, grep, LSP symbols, GitHub) and terminals |
| gitsigns.nvim | hunks in the sign column, stage/reset/blame |
| vim-tmux-navigator | `C-h/j/k/l` across windows and tmux panes |

Updates: `:lua vim.pack.update()`, review the diff, `:write` to apply, then
`chezmoi add ~/.config/nvim/nvim-pack-lock.json`.

## Languages

| Language | Server | Format on save |
| :--- | :--- | :--- |
| Python | basedpyright + ruff | ruff |
| TS / JS | vtsls (+ eslint with a config) | project prettier, else vtsls |
| C / C++ | clangd | clangd (clang-format) |
| SQL | — | sql-formatter |
| OCaml | ocamllsp | ocamlformat (with `.ocamlformat`) |
| Haskell | hls | ormolu |

`:FormatToggle` turns format-on-save off for the session; `<leader>cf`
formats on demand.

## Keys

Stock keys still apply: `grn` rename, `gra` code action, `grr` references,
`gri` implementation, `K` hover, `[d`/`]d` diagnostics, `gc` comment,
`<C-y>` accept completion, `<C-n>`/`<C-p>` move, `<Tab>` snippet jump.

| Key | Action |
| :--- | :--- |
| `<leader>ff` / `fg` / `fc` | Find files / grep / grep word |
| `<leader>fb` / `fr` / `f/` / `f.` | Buffers / recent / search buffer / resume |
| `<leader>fh` / `fk` / `fd` | Help / keymaps / diagnostics |
| `<leader>fs` / `fS` | Document / workspace symbols |
| `<leader>fD` | Docs for word (hoogle, ocaml.org, devdocs, docs.rs) |
| `-` | Oil: parent directory |
| `gd` / `gD` | Definition / declaration |
| `<leader>la` / `lr` / `ld` / `lh` | Code action / rename / line diagnostics / inlay hints |
| `<leader>ci` / `cF` / `cE` | Organize imports / fix all / fix ESLint |
| `<leader>cf` | Format |
| `<C-Space>` (insert) | Force completion |
| `]h` / `[h` | Next / previous hunk |
| `<leader>gs` / `gr` / `gS` / `gR` | Stage / reset hunk / buffer |
| `<leader>gp` / `gb` / `gd` | Preview hunk / blame / diff |
| `<leader>gg` / `gi` / `gP` / `gB` | LazyGit / issues / PRs / open on GitHub |
| `<leader>ob` / `on` | Build / test project (terminal) |
| `<leader>os` / `od` | Run package.json script / `dev` |
| `<leader>op` / `oT` / `oC` | Run Python file / pytest / compile C/C++ file |
| `<localleader>r` | REPL: utop, ghci/cabal repl, ipython |
| `<localleader>e` / `b` / `f` | Send line or selection / buffer / top-level form |
| `<localleader>w` / `W` | Rebuild / re-test on save |
| `<localleader>s`, `]H` / `[H` | OCaml: .ml/.mli switch, typed holes |
| `<C-/>` | Terminal (right) |
| `<leader>aa` / `ac` | Terminal agent / copy context reference |
| `<leader>uC` | Theme picker (built-in colorschemes; pin light/dark) |
| `<leader>uu` | Undo tree |
| `<leader><tab>s` / `o` | Save / open session for this directory |

## Validation

```sh
nvim --headless +qa
nvim --headless '+checkhealth vim.deprecated vim.pack' +qa
```
