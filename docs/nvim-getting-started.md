# Neovim — Getting Started (Hands-On)

A do-along lesson for *this* configuration: near-stock Neovim 0.12 with the
built-in LSP client, **snacks** (find), **oil** (files), **blink.cmp**
(completion), **conform** (formatting) and **gitsigns + codediff** (review).
Open Neovim in a real project and work through each section; every one ends
with a **Practice**. The full key list is in [`nvim.md`](nvim.md).

The single most important key: **`<Esc>`** — it returns you to Normal mode
from anywhere. Mistake? `u` undoes it.

---

## How to read the keys

- Neovim is **modal**: **Normal** mode (navigate, run commands), **Insert**
  (`i` to type), **Visual** (`v` to select). `<Esc>` returns to Normal.
  Everything below is pressed in Normal mode unless it says otherwise.
- **`<leader>` is `Space`**; `<leader>ff` means `Space`, `f`, `f`.
  **`<localleader>` is `,`**.
- `<C-s>` = `Ctrl-s`. `]h` = `]` then `h`.
- There is no which-key popup. To look a key up, use **`<leader>fk`**: a
  fuzzy list of every mapping with its description.

> **Practice** — `<leader>fk`, type `hunk`, read the git-hunk keys. `<Esc>` closes.

---

## 0. The leader groups

| `Space` + | Group |
| :--- | :--- |
| `f` | **Find** (files, grep, symbols, help, keymaps) |
| `l` | **LSP** (action, rename, diagnostics, hints, restart) |
| `c` | **Code** (format, imports, fix-all) |
| `g` | **Git** (hunks, blame, diff view, history, LazyGit, GitHub) |
| `o` | **Build / run / test** |
| `d` | **Debug** |
| `x` | **Diagnostics** lists |
| `b` / `s` / `<Tab>` | Buffers / splits / tabs and sessions |
| `u` | UI (theme picker, undo tree, line blame) |
| `a` | Terminal agent |

Single keys worth knowing now: `-` file explorer, `<leader>,` buffers,
`<leader>/` grep, `<leader>'` reopen the last picker, `<leader>n`
notifications, `<C-/>` terminal, `<leader>q` quit.

---

## 1. Files, windows, buffers

**Oil** shows a directory as a text buffer in a floating window, with a
preview of the entry under the cursor. Create, rename or delete files by
editing the text, then `:w`. Renames update imports through the LSP.

| Key | Action |
| :--- | :--- |
| `-` | Open the parent directory (press again to go up) |
| `<CR>` | Open the file or directory under the cursor |
| `q` / `<C-c>` | Close oil |
| `g?` | Oil's own key list |

**Find and open files** (snacks picker):

| Key | Action |
| :--- | :--- |
| `<leader>ff` | Find files (hidden files included) |
| `<leader>fr` | Recent files |
| `<leader>fb` or `<leader>,` | Open buffers |

**Windows and buffers:**

| Key | Action |
| :--- | :--- |
| `<leader>sv` / `<leader>sh` | Split vertical / horizontal |
| `<leader>se` / `<leader>sx` | Equalize / close split |
| `<C-h/j/k/l>` | Move between windows (and tmux panes) |
| `<C-↑/↓/←/→>` | Resize the window |
| `<leader>bn` / `bp` / `bd` / `bh` | Next / previous / delete buffer / close hidden ones |
| `<C-s>` / `<leader>q` | Save / quit |

> **Practice** — `<leader>ff` open a file, `-` to see its folder in oil, `q` to close it, `<leader>sv` to split, `<C-l>` to hop right.

---

## 2. Moving fast

| Key | Action |
| :--- | :--- |
| `<leader>f/` | Fuzzy-search the current buffer |
| `<leader>fs` (or stock `gO`) | Jump to a symbol in this file |
| `]]` / `[[` | Next / previous use of the word under the cursor |
| `]d` / `[d` | Next / previous diagnostic |
| `]h` / `[h` | Next / previous git hunk |
| `]m` / `[m` | Next / previous function (`]M`/`[M` its end) |
| `]k` / `[k` | Next / previous class |
| `<C-d>` / `<C-u>` | Half-page down / up (kept centered) |
| `<C-o>` / `<C-i>` | Back / forward through jumps |

> **Practice** — Put the cursor on a variable, press `]]` a few times, then `<C-o>` to come back.

---

## 3. Finding and searching the project

| Key | Finds |
| :--- | :--- |
| `<leader>fg` or `<leader>/` | **Live grep** across the project (hidden files included) |
| `<leader>fc` | Grep the word under the cursor (or the selection) |
| `<leader>f.` or `<leader>'` | Reopen the last picker |
| `<leader>fh` / `<leader>fk` | Help tags / keymaps |
| `<leader>fS` | Workspace symbols |

Inside a picker: **type** to filter, `<C-j>`/`<C-k>` (or `<C-n>`/`<C-p>`)
to move, `<CR>` to open, `<C-v>`/`<C-s>` to open in a vertical/horizontal
split, `<C-t>` in a tab, `<C-q>` to send the results to the quickfix list,
`<Esc>` to close.

> **Practice** — `<leader>fg`, type a function name used across files, `<CR>` to jump. Then `<leader>'` to reopen the same search.

---

## 4. Editing

- **Pairs:** typing `(`, `[`, `{`, `"`, `'` or `` ` `` inserts the closer;
  typing the closer steps over it; `<BS>` deletes both. Works in the `:`
  command line too.
- **Comment:** `gcc` toggles a line; `gc{motion}` or `gc` in Visual mode a block.
- **Text objects** (after `d`, `c`, `y`, `v`…): `af`/`if` function,
  `ac`/`ic` class, `aa`/`ia` argument, `au`/`iu` call, `ao`/`io` block,
  `ii`/`ai` indent scope, plus brackets and quotes (`ci(` works even from
  before the parens).
- **Surround:** `gsaiw"` wraps a word in quotes, `gsd(` removes parens,
  `gsr'"` swaps `'` for `"`.
- **Select by syntax:** in Visual mode `an` grows the selection to the
  parent node, `in` shrinks it; `]n`/`[n` move to the next/previous node.
- **Move lines:** `J`/`K` in Visual mode move the selection down/up;
  `<`/`>` indent and keep it selected.
- **Paste over a selection** without losing the register: `<leader>p`.
- **Undo tree** (`<leader>uu`): browse and restore any earlier state.

> **Practice** — Inside a function, `vaf` selects all of it and `yif` copies its body. On a word, `gsaiw)` wraps it in parens and `gsd)` takes them off. Type `foo(` in insert mode and watch `)` appear.

---

## 5. Completion (blink.cmp)

Suggestions appear as you type, from the language server, file paths,
snippets and the current buffer. Docs for the selected item show after
200ms. Accepting a function or method adds the `(`.

| Key | Action |
| :--- | :--- |
| `<CR>` | Accept |
| `<C-n>` / `<C-p>` (or arrows) | Next / previous candidate |
| `<C-Space>` | Open the menu, or toggle the docs |
| `<C-e>` | Close the menu |
| `<C-b>` / `<C-f>` | Scroll the docs |
| `<C-k>` | Toggle signature help |
| `<Tab>` / `<S-Tab>` | Accept the inline suggestion if one shows; else jump through snippet placeholders |

In the `:` command line the menu opens by itself as you type.

**Inline suggestions (Copilot, optional):** run `:LspCopilotSignIn` once
(needs a GitHub Copilot plan; your code goes to GitHub once signed in). Grey
ghost text then appears as you type: `<Tab>` accepts, `<M-]>`/`<M-[>` cycle,
`<leader>uA` turns them off or on.

> **Practice** — In a code buffer start typing a known function, `<C-n>` to it, `<CR>` to accept — note the `(` it adds.

---

## 6. Code intelligence (built-in LSP)

Servers come from mason (installed on first start) or the toolchain (clangd,
ocamllsp, hls, jdtls) and attach by filetype: basedpyright + ruff, vtsls,
eslint, oxlint, cssls, tailwindcss, lua_ls, rust-analyzer, gopls, bashls
(with shellcheck), jsonls and yamlls (schemas for package.json, tsconfig,
GitHub workflows, docker-compose…), taplo, marksman, jdtls, clangd,
ocamllsp, hls. Inlay hints are on where supported.

| Key | Action |
| :--- | :--- |
| `gd` / `gD` | Definition / declaration (`<C-o>` jumps back) |
| `grr` / `gri` / `grt` | References / implementation / type definition |
| `K` | Hover documentation |
| `<leader>la` (or `gra`) | Code action |
| `<leader>lr` (or `grn`) | Rename |
| `<leader>ld` | Line diagnostics (float) |
| `<leader>ls` / insert `<C-s>` | Signature help |
| `<leader>lh` | Toggle inlay hints |
| `<leader>lR` | Restart the language servers |
| `<leader>fD` | Real docs for the word (hoogle, ocaml.org, devdocs, docs.rs) |

**Imports and cleanup** (`<leader>c`): `ci` organize, `cI` add missing,
`cu` remove unused, `cF` fix all, `cE` ESLint fixes.

**Diagnostics lists:** `<leader>xx` all buffers, `<leader>xb` this buffer,
`<leader>fd` the picker.

> **Python:** basedpyright (types) and ruff (lint, fix, format) find the project's `.venv`; `:PythonEnv` shows which interpreter they use.

> **Practice** — `gd` on a symbol, `<C-o>` back. `<leader>lr` to rename it, `<leader>la` to see quick-fixes.

---

## 7. Formatting (automatic)

**Format on save** is on (conform.nvim, 1000ms timeout). Web files use the
first installed of oxfmt, prettierd, prettier; Lua stylua; shell shfmt;
fish fish_indent; SQL sql-formatter; everything else its language server
(ruff, gopls, rustfmt, clang-format, ocamlformat, ormolu). Go organizes
imports first.

- `<leader>cf` formats now (the selection in Visual mode).
- `:ToggleFormatter` turns format on save off, or back on, for this buffer.
- `:ConformInfo` shows which formatter a buffer uses.

> **Practice** — Add stray spaces to a line and press `<C-s>` — watch it reformat.

---

## 8. Build, run, test

Commands run in a terminal split, chosen from the project's root markers
(Cargo, go.mod, dune, cabal/stack, package.json, pyproject/uv, CMake/Make,
Maven/Gradle).

| Key | Runs |
| :--- | :--- |
| `<leader>ob` / `<leader>on` | Build / test the project |
| `<leader>op` / `<leader>oT` | Run the current Python file / pytest |
| `<leader>oC` | Compile the current C/C++ file |
| `<leader>os` / `<leader>od` | Pick a package.json script / run `dev` |
| `<localleader>w` / `W` | Rebuild / re-test on every save |

**REPL** (`<localleader>r` toggles): utop, ghci/cabal repl, ipython, racket,
sml, jshell. `<localleader>e` sends the line or selection, `b` the buffer,
`f` the top-level form.

**Debug** (Go via Delve, Python via debugpy): `<F5>` start, `<leader>db`
breakpoint, `<F10>`/`<F11>`/`<F12>` step, `<leader>dt` debug the test under
the cursor (Go test, or pytest test in Python).

> **Practice** — In a project, `<leader>on` to run the tests; `<C-/>` toggles a terminal on the right.

---

## 9. Git

- **`<leader>gg` → LazyGit**, the terminal UI for commit / push / branch /
  rebase.
- **Hunks** (gitsigns): `]h`/`[h` jump; `<leader>gp` preview, `<leader>gs`
  stage, `<leader>gr` reset (Visual mode: just the selected lines);
  `<leader>gS`/`gR` the whole buffer; `<leader>gh` a picker of every hunk.
- **Blame:** `<leader>gb` popup for the line, `<leader>ub` toggles
  end-of-line blame, `<leader>ga` blames the whole file.
- **Diff view** (codediff): `<leader>gv` shows changed files as a tree with
  `+/-` counts; moving the cursor opens each diff side by side. `gc` folds
  everything but the changes. `<leader>gl` repository history,
  `<leader>gL` this file's history (or the selected lines').
- **GitHub:** `<leader>gB` open the file/selection, `<leader>gi` issues,
  `<leader>gP` pull requests.

> **Practice** — Edit a tracked file, `<leader>gv` to review it, `-` on the file in the explorer to stage it, then `<leader>gg` to commit.

---

## 10. Discover, don't memorize

- `<leader>fk` searches every keymap; `<leader>fh` searches help.
- `:checkhealth` diagnoses problems; `:checkhealth vim.lsp` shows attached
  servers; `:Mason` lists installed tools.
- `<leader>n` shows recent notifications; `g<` reopens the last long message.

---

## 11. When something feels broken

- **Stuck** in a half-typed command or the wrong mode: `<Esc>`. Bad edit:
  `u` (or `<leader>uu`).
- **No completion or formatting** for a language: `:checkhealth vim.lsp`
  (did the server attach?), `:Mason` (is it installed?), `:ConformInfo`
  (which formatter?), `<leader>lR` to restart servers.
- **A plugin missing:** `:lua vim.pack.update()`, review, `:write`.
- **After changing the config:** `ZR` restarts Neovim and restores the
  session.
- **General health:** `:checkhealth`.

---

## One-page cheat sheet

| Want to… | Press |
| :--- | :--- |
| Back to safety | `<Esc>` (then `u`) |
| Look up a key | `<leader>fk` |
| Find file / recent | `<leader>ff` / `<leader>fr` |
| Search text | `<leader>fg` or `<leader>/` |
| File explorer | `-` |
| Go to definition | `gd` (back: `<C-o>`) |
| Hover docs | `K` |
| Rename / code action | `<leader>lr` / `<leader>la` |
| Format | `<leader>cf` (auto on save) |
| Build / test | `<leader>ob` / `<leader>on` |
| Review changes | `<leader>gv` |
| Git UI | `<leader>gg` |
| Diagnostics list | `<leader>xx` |
| Terminal | `<C-/>` |

---

## Where to go next

- **[Neovim Development Workflow](nvim-dev-workflow.md)** — the daily read / edit / run / review loop.
- **[Neovim Research Workflow](nvim-research-workflow.md)** — Python, Markdown and SQL for research work.
- **[Neovim Note-Taking Workflow](nvim-note-taking-workflow.md)** — Markdown notes next to code.
- **[Neovim Starting Projects](nvim-starting-projects.md)** — project shapes that make the tooling attach.
- **[Neovim Setup Overview](nvim.md)** — every plugin and key.
