# Neovim Setup

Near-stock Neovim 0.12: the stock look and keys, built-in LSP, snippets,
comments and undotree, blink.cmp for completion, a few plugins for what Neovim cannot do
itself, and one theme (lsdr/monokai). Tooling covers **Python, TypeScript/JavaScript, C/C++, SQL, OCaml,
Haskell and Go**.

## Structure

```text
init.lua                  vim.loader, leaders (<Space>, local leader ",")
nvim-pack-lock.json       plugin revisions (vim.pack)
after/lsp/<server>.lua    overrides on nvim-lspconfig's defaults
after/ftplugin/           per-filetype indentation and OCaml LSP extras
lua/naamanu/
  theme.lua               shared light/dark mode + built-in colorscheme picker
  core/
    init.lua              load order
    options.lua           options, :grep via rg, ui2
    keymaps.lua           general keys, sessions, REPL, build/test keys
    autocmds.lua
    plugins.lua           the only plugin list (vim.pack) and its setup
    lsp.lua               diagnostics, LspAttach keys, capabilities, server list
    format.lua            format on save (conform.nvim, LSP fallback)
    dap.lua               debugging (nvim-dap + Delve)
    tasks.lua             project build/test/watch commands and the REPL
    workflows.lua         terminal agent, docs lookup
```

## Plugins

| Plugin | Why |
| :--- | :--- |
| nvim-treesitter (`main`) | parsers for python, ts/tsx/js, json, sql, ocaml, haskell, c/cpp, go |
| oil.nvim | directories as editable buffers (`-`) |
| snacks.nvim | picker (files, grep, LSP symbols, GitHub) and terminals |
| gitsigns.nvim | hunks in the sign column, stage/reset/blame |
| nvim-dap | debugging; Go through Delve's DAP server (`dlv dap`) |
| blink.cmp (`^1`) | completion menu; `<CR>` accepts, opens by itself in the cmdline |
| mini.pairs | auto-closing brackets and quotes in insert and cmdline mode |
| conform.nvim | format on save, 1000ms timeout, LSP fallback |
| vim-tmux-navigator | `C-h/j/k/l` across windows and tmux panes |
| monokai (lsdr) | the original TextMate Monokai, as shipped |

Updates: `:lua vim.pack.update()`, review the diff, `:write` to apply, then
`chezmoi add ~/.config/nvim/nvim-pack-lock.json`.

## Languages

| Language | Server | Format on save |
| :--- | :--- | :--- |
| Python | basedpyright + ruff | ruff |
| TS / JS / JSON / CSS / HTML / Markdown / YAML | vtsls, cssls (+ eslint with a config) | first installed of oxfmt, prettierd, prettier; else the server |
| Lua | lua_ls | stylua |
| sh / bash | — | shfmt |
| fish | — | fish_indent |
| C / C++ | clangd | clangd (clang-format) |
| SQL | — | sql-formatter |
| OCaml | ocamllsp | ocamlformat (with `.ocamlformat`) |
| Haskell | hls | ormolu |
| Go | gopls | gofumpt (via gopls), imports organized first |

`:ToggleFormatter` turns format on save off (and back on) for the current
buffer; `<leader>cf` formats on demand.

## Keys

Stock keys still apply: `grn` rename, `gra` code action, `grr` references,
`gri` implementation, `K` hover, `[d`/`]d` diagnostics, `gc` comment,
Completion (blink.cmp, "enter" preset): `<CR>` accept, `<C-n>`/`<C-p>`
move, `<C-e>` close, `<Tab>`/`<S-Tab>` snippet jump.

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
| `<C-Space>` (insert) | Open completion / toggle docs |
| `]h` / `[h` | Next / previous hunk |
| `<leader>gs` / `gr` / `gS` / `gR` | Stage / reset hunk / buffer |
| `<leader>gp` / `gb` / `gd` | Preview hunk / blame line (popup) / diff |
| `<leader>ga` | Blame the whole file (side window; `<CR>` opens the commit) |
| `<leader>ub` | Toggle end-of-line blame for the cursor line (off at start) |
| `<leader>gg` / `gi` / `gP` / `gB` | LazyGit / issues / PRs / open on GitHub |
| `<leader>ob` / `on` | Build / test project (terminal) |
| `<leader>os` / `od` | Run package.json script / `dev` |
| `<leader>op` / `oT` / `oC` | Run Python file / pytest / compile C/C++ file |
| `<localleader>r` | REPL: utop, ghci/cabal repl, ipython |
| `<localleader>e` / `b` / `f` | Send line or selection / buffer / top-level form |
| `<localleader>w` / `W` | Rebuild / re-test on save |
| `<localleader>s`, `]H` / `[H` | OCaml: .ml/.mli switch, typed holes |
| `<F5>` / `<leader>dc` | Debug: start (pick a configuration) / continue |
| `<F10>` / `<F11>` / `<F12>` | Step over / into / out (also `<leader>dn` / `di` / `do`) |
| `<leader>db` / `dB` / `dC` | Breakpoint / conditional breakpoint / run to cursor |
| `<leader>dt` | Debug the Go test under the cursor |
| `<leader>dl` / `dq` | Re-run last / terminate |
| `<leader>dr` / `ds` / `dh` | REPL / scopes / hover value |
| `<C-/>` | Terminal (right) |
| `<leader>aa` / `ac` | Terminal agent / copy context reference |
| `<leader>uC` | Theme picker (built-in colorschemes + Monokai; pin light/dark) |
| `<leader>uu` | Undo tree |
| `<leader><tab>s` / `o` | Save / open session for this directory |

## Validation

```sh
nvim --headless +qa
nvim --headless '+checkhealth vim.deprecated vim.pack' +qa
```
