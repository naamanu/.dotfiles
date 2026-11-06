# Neovim Setup

Near-stock Neovim 0.12: stock keys, the built-in LSP client, snippets,
comments, undotree and ui2 messages, plus a short list of plugins for what
Neovim cannot do itself (completion, formatting, pickers, git review). The
theme is `colors/monokai-pro.lua`, built from Ghostty's Monokai Pro palette;
`<leader>uC` picks another. Tooling covers **Python, TypeScript/JavaScript,
Go, Rust, C/C++, Java, Lua, SQL, OCaml and Haskell**, plus shell, JSON, YAML,
TOML and Markdown, with build/test and a REPL for Racket and Standard ML as
well.

Leaders: `<leader>` is `Space`, `<localleader>` is `,`.

## Structure

```text
init.lua                  vim.loader, leaders
nvim-pack-lock.json       plugin revisions (vim.pack)
colors/monokai-pro.lua    the default colorscheme
after/lsp/<server>.lua    overrides on nvim-lspconfig's defaults
after/ftplugin/           per-filetype indentation and OCaml extras
lua/naamanu/
  theme.lua               shared light/dark mode + colorscheme picker
  core/
    init.lua              load order (each module in its own pcall)
    options.lua           options, :grep via rg, ui2 message routing
    keymaps.lua           general keys, buffers, splits, tabs, sessions
    autocmds.lua          yank flash, restore cursor, auto-create dirs, reload
    plugins.lua           the only plugin list (vim.pack), mason, pairs,
                          text objects, surround, completion, treesitter
    ui.lua                icons, cmdline, lualine, incline, oil, snacks
    git.lua               gitsigns, codediff
    tabline.lua           tabline (shown with 2+ tabs)
    lsp.lua               diagnostics, LspAttach keys, capabilities, servers,
                          inline suggestions
    format.lua            format on save (conform.nvim)
    dap.lua               debugging (nvim-dap: Delve, debugpy), loaded on use
    tasks.lua             build/test/watch commands and the REPL
    workflows.lua         terminal agent, docs lookup
```

## Plugins

| Plugin | Why |
| :--- | :--- |
| nvim-treesitter (`main`) | ~40 parsers: highlighting, folds, indent where a query exists |
| nvim-treesitter-textobjects (`main`) | function/class queries for mini.ai; `]m`/`[m`, `]k`/`[k` moves |
| nvim-lspconfig | default server configs; ours override them from `after/lsp/` |
| SchemaStore.nvim | JSON/YAML schema catalog for jsonls and yamlls |
| mason.nvim + mason-tool-installer | installs servers, formatters and debugpy (never auto-updates; `:MasonToolsUpdate`) |
| blink.cmp (`^1`) | completion as you type, in buffers and the cmdline |
| conform.nvim | format on save, 1000ms timeout, LSP fallback |
| mini.pairs | auto-closing brackets and quotes in insert and cmdline mode |
| mini.ai | a/i text objects: function, class, block, call, argument, brackets, quotes |
| mini.surround | add/delete/replace surrounding pairs on `gs` |
| mini.icons | file and LSP-kind icons (also stands in for nvim-web-devicons) |
| snacks.nvim | picker, terminal, notifications, input, indent guides and scope, statuscolumn, big-file guard, `]]`/`[[` reference jumps |
| oil.nvim | directories as editable buffers, in a float with a preview (`-`) |
| gitsigns.nvim | hunks in the sign column, stage/reset, blame |
| codediff.nvim | side-by-side diffs and history with a changed-files explorer |
| nvim-dap (+ nvim-dap-python) | debugging; Go through Delve (`dlv dap`), Python through debugpy |
| lualine.nvim | one global statusline |
| incline.nvim | file name tag in each window's top-right corner |
| tiny-cmdline.nvim | the ui2 cmdline as a centered floating box |
| todo-comments.nvim (+ plenary.nvim) | TODO/FIXME/HACK highlighted |
| nvim-highlight-colors | color literals shown in their own color |
| vim-tmux-navigator | `C-h/j/k/l` across windows and tmux panes |

Plus the built-in `nvim.undotree` package. Updates: `:lua vim.pack.update()`,
review, `:write` to apply, then `chezmoi add ~/.config/nvim/nvim-pack-lock.json`.

## Languages

| Language | Server | Format on save |
| :--- | :--- | :--- |
| Python | basedpyright + ruff | ruff (LSP) |
| TS / JS / JSX / TSX | vtsls, oxlint (+ eslint with a config) | first installed of oxfmt, prettierd, prettier |
| CSS / SCSS / Less / HTML / MDX | cssls, tailwindcss where they apply | first installed of oxfmt, prettierd, prettier |
| JSON / YAML | jsonls / yamlls, schemas from SchemaStore | first installed of oxfmt, prettierd, prettier |
| Markdown | marksman | first installed of oxfmt, prettierd, prettier |
| Go | gopls | imports organized, then gofumpt (gopls) |
| Rust | rust-analyzer | rustfmt (LSP) |
| C / C++ | clangd (Xcode/system) | clang-format (LSP) |
| Java | jdtls (Homebrew) | Eclipse formatter (LSP) |
| Lua | lua_ls | stylua |
| sh / bash | bashls + shellcheck | shfmt |
| fish | — | fish_indent |
| SQL | — | sql-formatter |
| TOML | taplo | taplo (LSP) |
| OCaml | ocamllsp (opam) | ocamlformat (with `.ocamlformat`) |
| Haskell | hls (ghcup/brew) | ormolu |

Anything without a formatter listed falls back to its language server.
`:ToggleFormatter` turns format on save off (and back on) for the current
buffer; `<leader>cf` formats on demand. Racket and Standard ML have no
language server here; they get build/test (`<leader>ob`/`on`) and a REPL.
Server color swatches are off; nvim-highlight-colors paints colors instead.

jdtls runs on Homebrew's newest `openjdk` (current releases need JDK 25+) and
checks code against `$JAVA_HOME` (JDK 21); see `after/lsp/jdtls.lua`.

## Completion

blink.cmp with the "enter" preset. The menu opens as you type; docs show
after 200ms; accepting a function adds `(`; no ghost text. In the `:` and `/`
cmdline the menu opens by itself too.

| Key (insert) | Action |
| :--- | :--- |
| `<CR>` | Accept |
| `<C-n>` / `<C-p>`, `<Down>` / `<Up>` | Next / previous item |
| `<C-Space>` | Open the menu / toggle docs |
| `<C-e>` | Close the menu |
| `<C-b>` / `<C-f>` | Scroll docs |
| `<C-k>` | Toggle signature help |
| `<Tab>` / `<S-Tab>` | Accept the inline suggestion if one shows, else jump between snippet fields |

### Inline suggestions (Copilot)

The copilot language server (mason) shows suggestions as ghost text through
the built-in `vim.lsp.inline_completion`. It does nothing until you run
`:LspCopilotSignIn` once on a machine (needs a GitHub Copilot plan; code is
sent to GitHub once signed in).

| Key | Action |
| :--- | :--- |
| `<Tab>` (insert) | Accept the suggestion |
| `<M-]>` / `<M-[>` (insert) | Next / previous suggestion |
| `<leader>uA` | Inline suggestions off / on |

## Keys

Stock keys still apply: `grn` rename, `gra` code action, `grr` references,
`gri` implementation, `grt` type definition, `gO` symbols, `K` hover,
`[d`/`]d` diagnostics, `gc`/`gcc` comment, `<C-s>` (insert) signature help.
In visual mode `an`/`in` grow/shrink the selection by syntax node. `ZR`
restarts Neovim and restores the session.

Text objects (after an operator or in Visual mode; `ci(` works from before
the parens too):

| Object | Selects |
| :--- | :--- |
| `af` / `if` | Function |
| `ac` / `ic` | Class |
| `ao` / `io` | Block, conditional or loop |
| `au` / `iu` | Function call |
| `aa` / `ia` | Argument |
| `ai` / `ii` | Indent scope |
| `a(` `a[` `a{` `a"` `at` … | Brackets, quotes, tags (mini.ai) |

Surround (mini.surround): `gsa{motion}{char}` adds (`gsaiw"` quotes a word),
`gsd{char}` deletes, `gsr{old}{new}` replaces, `gsf`/`gsF` find, `gsh`
highlights.

| Key | Action |
| :--- | :--- |
| **Find** | |
| `<leader>ff` / `fg` / `fc` | Find files / grep / grep word or selection |
| `<leader>,` / `<leader>/` / `<leader>'` | Buffers / grep / reopen last picker |
| `<leader>fb` / `fr` / `f/` / `f.` | Buffers / recent / search buffer / resume |
| `<leader>fh` / `fk` / `fd` | Help / keymaps / diagnostics |
| `<leader>fs` / `fS` | Document / workspace symbols |
| `<leader>fD` | Docs for word or selection (hoogle, ocaml.org, devdocs, docs.rs) |
| `<leader>xx` / `xb` | Diagnostics, all buffers / this buffer |
| `]m` / `[m`, `]M` / `[M` | Next / previous function start, end |
| `]k` / `[k` | Next / previous class |
| `]i` / `[i` | Edges of the indent scope |
| `<leader>n` | Notification history |
| **Files and windows** | |
| `-` | Oil: parent directory (float; `q` closes) |
| `<C-s>` / `<leader>q` | Save / quit |
| `<leader>sv` / `sh` / `se` / `sx` | Split vertical / horizontal / equalize / close |
| `<leader>bn` / `bp` / `bd` / `bh` | Next / previous / delete buffer / close hidden buffers |
| `<leader><tab>n` / `x` / `]` / `[` / `l` / `r` | New / close / next / previous / last / move tab |
| `<leader><tab>s` / `o` | Save / open session for this directory |
| `<leader>p` (visual) | Paste without yanking |
| **Code** | |
| `gd` / `gD` | Definition / declaration |
| `<leader>la` / `lr` / `ld` / `ls` | Code action / rename / line diagnostics / signature help |
| `<leader>lh` / `lc` / `lR` | Toggle inlay hints / run code lens / restart servers |
| `<leader>ci` / `cI` / `cu` | Organize / add missing / remove unused imports |
| `<leader>cF` / `cE` | Fix all / fix ESLint |
| `<leader>cf` | Format |
| **Git** | |
| `]h` / `[h` | Next / previous hunk |
| `<leader>gs` / `gr` / `gS` / `gR` | Stage / reset hunk (or selection) / buffer |
| `<leader>gp` / `gd` / `gh` | Preview hunk / diff against index / hunks picker |
| `<leader>gb` / `ga` / `ub` | Blame line (popup) / blame file / toggle end-of-line blame |
| `<leader>gv` | Diff view of changed files (CodeDiff) |
| `<leader>gl` / `gL` | History / history of this file (or selected lines) |
| `<leader>gg` / `gi` / `gP` / `gB` | LazyGit / issues / PRs / open on GitHub |
| **Build, run, REPL** | |
| `<leader>ob` / `on` | Build / test project (terminal) |
| `<leader>os` / `od` | Run package.json script / `dev` |
| `<leader>op` / `oT` / `oC` | Run Python file / pytest / compile C/C++ file |
| `<localleader>r` | REPL: utop, ghci/cabal repl, ipython, racket, sml, jshell |
| `<localleader>e` / `b` / `f` | Send line or selection / buffer / top-level form |
| `<localleader>w` / `W` | Rebuild / re-test on save |
| `<localleader>s`, `]H` / `[H` | OCaml: .ml/.mli switch, typed holes |
| **Debug** | |
| `<F5>` / `<leader>dc` | Start (pick a configuration) / continue |
| `<F10>` / `<F11>` / `<F12>` | Step over / into / out (also `<leader>dn` / `di` / `do`) |
| `<leader>db` / `dB` / `dC` | Breakpoint / conditional breakpoint / run to cursor |
| `<leader>dt` | Debug the test under the cursor (Go; Python with pytest) |
| `<leader>dl` / `dq` | Re-run last / terminate |
| `<leader>dr` / `ds` / `dh` | REPL / scopes / hover value |
| **Other** | |
| `<C-/>` | Terminal (right) |
| `<leader>aa` / `ac` | Terminal agent / copy context reference |
| `<leader>uC` | Theme picker (live preview; pin light/dark) |
| `<leader>uA` | Inline (AI) suggestions off / on |
| `<leader>uu` | Undo tree |

Inside a CodeDiff view its own keys apply: `]c`/`[c` hunks, `]f`/`[f`
files, `-` stage file, `gc` compact mode (8 context lines, folds synced),
`<leader>b`/`<leader>e` toggle/focus the explorer.

## Validation

```sh
nvim --headless +qa
nvim --headless '+checkhealth vim.deprecated vim.pack' +qa
```
