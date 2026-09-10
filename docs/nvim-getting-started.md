# Neovim — Getting Started (Hands-On)

A do-along lesson for *this* configuration: a `lazy.nvim` setup built around
**Telescope** (find), **Oil** (files), **native LSP** (code), **Overseer** (tasks),
and **snacks/which-key** (glue) — for note-taking, research, and software
engineering. Open Neovim, keep a real project handy, and work through each section.
Every one ends with a **Practice**.

The single most important key: **`<Esc>`** — it returns you to Normal mode from
anywhere. If a sequence feels stuck, press `<Esc>` and you're safe. Mistake? `u`
undoes it.

---

## How to read the keys

- Neovim is **modal**: **Normal** mode (navigate + run commands), **Insert** mode
  (`i` to type), **Visual** mode (`v` to select). `<Esc>` always returns to Normal.
  Everything below is pressed in **Normal** mode unless it says "type".
- **`<leader>` is the `Space` key.** `<leader>ff` means: `Space`, then `f`, then `f`.
- `<C-s>` = `Ctrl-s`. `]h` = `]` then `h`.
- **You don't memorize — you browse.** Press `<leader>` (Space) and *pause*:
  **which-key** pops up every group and key. Same after `g`, `]`, `[`, etc.

> **Practice** — Press `Space` and wait one second. Read the which-key popup. Press `<Esc>` to dismiss.

---

## 0. The shape of this config — the leader groups

Almost every command lives under a `Space` group. Learn the groups, then let
which-key fill in the rest:

| `Space` +  | Group                                    |
| :--------- | :--------------------------------------- |
| `f`        | **Find / search** (Telescope, Flash)     |
| `l`        | **LSP** (definitions, rename, diagnostics) |
| `c`        | **Code** (actions, format, imports)      |
| `g`        | **Git**                                  |
| `o`        | **Overseer** (build / run / test)        |
| `x`        | **Diagnostics** (Trouble)                |
| `b` / `s`  | Buffers / Splits                         |
| `R`        | **REST / HTTP** (Kulala, in `.http` files) |
| `n`        | Notifications / scratch                  |
| `r` / `u`  | Rename / UI toggles                      |

Standalone keys worth knowing now: `<leader>e` file explorer, `<leader>.` scratch
buffer, `<leader><leader>` jump anywhere (Flash), `<leader>j` split/join, `<leader>z`
zen mode, `<leader>uu` undo tree, `<leader>q` quit.

> **Practice** — Press `Space`, wait, then `f`. Read every find command. `<Esc>` out.

---

## 1. Files, windows, buffers

**Oil** turns a directory into an editable buffer — you create, rename, and delete
files by editing text and saving (`:w`):

| Key         | Action                              |
| :---------- | :---------------------------------- |
| `-`         | Open the **parent directory** in Oil |
| `<leader>e` | Open the file explorer (Oil)        |

**Find and open files** (Telescope):

| Key         | Action                                       |
| :---------- | :------------------------------------------- |
| `<leader>ff` | Find any file                               |
| `<leader>fp` | Project files (git-tracked + untracked)     |
| `<leader>fr` | Recently opened files                       |
| `<leader>fb` | Open buffers                                |

**Windows & buffers:**

| Key                  | Action                          |
| :------------------- | :------------------------------ |
| `<leader>sv` / `<leader>sh` | Split vertical / horizontal |
| `<leader>se` / `<leader>sx` | Equalize / close split      |
| `<C-h/j/k/l>`        | Move between windows             |
| `<C-↑/↓/←/→>`        | Resize the window                |
| `<leader>bn` / `<leader>bp` / `<leader>bd` | Next / prev / delete buffer |
| `<C-s>` / `<leader>q` | Save / quit                    |

> **Practice** — `<leader>ff` open a file, press `-` to see its folder in Oil, `<leader>sv` to split, `<C-l>` to hop into the right window.

---

## 2. Moving fast

| Key            | Action                                                   |
| :------------- | :------------------------------------------------------- |
| `<leader><leader>` | **Flash** — type 2 characters, then a label, to jump anywhere on screen |
| `<leader>f/`   | Fuzzy-search the current buffer                          |
| `<leader>fs`   | Jump to a function/symbol in this file                   |
| `]f` / `[f`    | Next / previous **function** (treesitter)                |
| `]c` / `[c`    | Next / previous **class**                                |
| `]r` / `[r`    | Next / previous use of the word under the cursor         |
| `<C-d>` / `<C-u>` | Half-page down / up (kept centered)                   |

> **Practice** — `<leader><leader>`, type two characters of a word across the screen, then press its label to teleport there.

---

## 3. Finding & searching the project (Telescope)

| Key          | Finds                                   |
| :----------- | :-------------------------------------- |
| `<leader>ff` / `<leader>fp` | All files / project files |
| `<leader>fg` | **Live grep** — search text across the project |
| `<leader>fc` | Grep the word under the cursor          |
| `<leader>f.` | Resume the last search                  |
| `<leader>fr` | Recent files                            |
| `<leader>fh` / `<leader>fk` | Search help / search all keymaps |

Inside any picker: **type** to filter, `<C-n>`/`<C-p>` (or arrows) to move, `<CR>` to
open, `<C-v>`/`<C-x>` to open in a vertical/horizontal split, `<C-t>` in a tab,
`<Esc>` to close. Press `<C-/>` to see every mapping for that picker.

> **Practice** — `<leader>fg`, type a function name used across files, `<CR>` to jump to a hit. Then `<leader>f.` to reopen that same search.

---

## 4. Editing power moves

- **Insert text:** `i` (before cursor), `a` (after), `o` (new line). Brackets, quotes,
  and HTML/JSX tags auto-close as you type.
- **Surround** (`nvim-surround`): `ys{motion}{char}` adds — e.g. `ysiw"` wraps a word in
  quotes; `cs"'` changes `"` to `'`; `ds"` deletes surrounding quotes.
- **Comment:** `gcc` toggles a line; `gc{motion}` or `gc` in Visual toggles a block.
- **Split / join** (`<leader>j`): collapse a multi-line array/object/args into one
  line, or expand it back. Toggles on the node under the cursor.
- **Structural text objects:** combine an operator (`d`/`c`/`v`/`y`) with `af`/`if`
  (a / inside **function**) or `ac`/`ic` (a / inside **class**). So `daf` deletes a
  function, `cif` changes its body, `vac` selects a class.
- **Grow a selection by syntax:** `<C-space>` to start/expand to the next node, `<bs>`
  to shrink.
- **Undo tree** (`<leader>uu`): browse and restore *any* past state — undo history is
  saved across restarts.

> **Practice** — Put the cursor inside a function and press `vif` to select its body. Then `ysiw)` to wrap a word in parens, and `<leader>j` to expand a one-line table.

---

## 5. Completion (blink.cmp)

Suggestions appear as you type (from the LSP, file paths, snippets, the current
buffer, and the Neovim API in Lua files):

| Key            | Action                              |
| :------------- | :---------------------------------- |
| `<C-j>` / `<C-k>` | Next / previous candidate        |
| `<CR>`         | Accept                              |
| `<C-Space>`    | Force the menu open                 |
| `<C-e>`        | Cancel                              |
| `<Tab>` / `<S-Tab>` | Jump forward / back through snippet placeholders |
| `<C-b>` / `<C-f>` | Scroll the documentation popup   |

Function signatures show automatically while you fill in arguments.

> **Practice** — In a code buffer start typing a known symbol; `<C-j>` to a candidate, `<CR>` to accept.

---

## 6. Code intelligence (native LSP)

Language servers are installed by setup.sh and resolved from the shared PATH and attach by filetype (clangd,
rust-analyzer, basedpyright + `ruff` for Python, ocamllsp, hls, vtsls, eslint, json,
yaml, millet for SML, racket-langserver…). Each server is a file in `lsp/<name>.lua`
and starts only when its binary is on PATH. Inlay hints turn on where supported.

| Key          | Action                              |
| :----------- | :---------------------------------- |
| `gd` / `gD`  | Go to definition / declaration (`<C-o>` jumps back) |
| `grr`        | References (Neovim default)         |
| `gri` / `grt` | Implementation / type definition (Neovim defaults) |
| `K`          | Hover documentation (Neovim default) |
| `<leader>ls` | Signature help                      |
| `<leader>la` | Code action (quick-fixes)           |
| `<leader>lr` | **Rename** — live preview as you type |
| `<leader>ld` | Line diagnostics (float)            |
| `<leader>lh` | Toggle inlay hints (this buffer; `<leader>ui` for all) |
| `]d` / `[d`  | Next / previous diagnostic          |
| `<leader>lR` | Restart the language server         |

**Import & cleanup actions** (the `<leader>c` group):

| Key | Action |
| :-- | :----- |
| `<leader>ci` / `<leader>cI` | Organize / add-missing imports |
| `<leader>cu` / `<leader>cF` | Remove unused / fix-all        |
| `<leader>cE`               | Apply ESLint fixes             |

**Project-wide diagnostics** (Trouble): `<leader>xx` all, `<leader>xX` this buffer,
`<leader>cs` a symbol outline, `<leader>cl` a refs/defs panel.

> **Python:** `ruff` (lint/fix) and basedpyright (types) attach automatically and resolve the
> project from `pyproject.toml` / `uv.lock`. Neovim's own Python host uses a dedicated
> uv venv at `~/.local/share/nvim/venv` — your project's `.venv` is what the servers use.

> **Practice** — `gd` on a symbol to jump to its definition, `<C-o>` to jump back. Then `<leader>lr` to rename it everywhere, and `<leader>la` to see available quick-fixes.

---

## 7. Format & lint (automatic)

- **Format on save** is on (`conform.nvim`): clang-format, rustfmt, ocamlformat, ruff,
  ormolu, stylua, shfmt, sql-formatter, and prettier (a project-local prettier is
  preferred when present). Format manually with `<leader>cf`.
- **Lint** runs on save and when you leave Insert mode (`nvim-lint`): `ruff` for
  Python, `shellcheck` for shell. Trigger manually with `<leader>ll`.

> **Practice** — Add stray spaces to a line and press `<C-s>` — watch it reformat. In a Python file, reference an undefined name and see the diagnostic appear.

---

## 8. Build, run, test (Overseer)

The `<leader>o` group runs tasks in a terminal panel; `<leader>ot` toggles the panel.

| Key          | Runs                                       |
| :----------- | :----------------------------------------- |
| `<leader>ob` | Build the project (language-aware)         |
| `<leader>on` | Project test suite                         |
| `<leader>op` / `<leader>oT` | Run current Python file / Python tests |
| `<leader>oC` | Compile the current C/C++ file             |
| `<leader>os` | Pick an npm/pnpm **package script**        |
| `<leader>od` / `<leader>oy` / `<leader>ol` / `<leader>of` | dev / typecheck / lint / format script |
| `<leader>or` / `<leader>oa` | Run any task / act on a running task (restart, stop, open) |

> **Practice** — In a project, `<leader>on` to run the tests, `<leader>ot` to watch the output panel, `<leader>oa` to restart the last task.

---

## 9. Git

- **`<leader>gg` → LazyGit** — the full terminal UI for stage / commit / push / branch
  / rebase. Use this for anything substantial.
- **Inline** (gitsigns): `]h` / `[h` jump between hunks; `<leader>gp` preview a hunk,
  `<leader>gs` stage it, `<leader>gr` reset it, `<leader>gb` blame the line. Whole
  buffer: `<leader>gS` stage, `<leader>gR` reset. Changes show in the gutter.
- **Diffview**: `<leader>go` open a side-by-side diff, `<leader>gh` file history,
  `<leader>gc` close.

> **Practice** — Edit a tracked file, `<leader>gp` to preview the change, `<leader>gs` to stage the hunk, then `<leader>gg` to commit in LazyGit.

---

## 10. Markdown & quick notes

- Open a `.md` file and **render-markdown** styles headings, code blocks, bullets, and
  `- [ ]` checkboxes inline — no separate preview window.
- `<leader>.` opens a **scratch buffer** for throwaway notes; `<leader>S` picks among
  scratch buffers.
- `<leader>z` is **zen mode** for distraction-free writing.

> **Practice** — `<leader>.`, write a markdown list with a couple of `- [ ]` checkboxes, and watch them render.

---

## 11. HTTP requests (Kulala)

In a `.http` / `.rest` file, the `<leader>R` group is a full REST client:

| Key          | Action                          |
| :----------- | :------------------------------ |
| `<leader>Rs` / `<leader>Ra` | Send request under cursor / send all |
| `<leader>Rt` | Toggle the response view        |
| `<leader>Rl` / `<leader>Rc` | Replay last / copy as curl |
| `<leader>R]` / `<leader>R[` | Next / previous request    |
| `<leader>Re` | Select the environment          |

> **Practice** — Make `test.http` containing `GET https://httpbin.org/get`, put the cursor on it, and press `<leader>Rs`.

---

## 12. Discover, don't memorize

- **which-key** — pause after any prefix (`Space`, `g`, `]`, `[`, `<leader>c`…) to see
  what's available.
- `<leader>fk` searches **every keymap**; `<leader>fh` searches the help.
- `:checkhealth` diagnoses problems · `:Lazy`
  manages plugins · `:LspInfo` shows what's attached to the buffer.

> **Practice** — `<leader>fk`, type `hunk` to surface every git-hunk binding at once.

---

## 13. When something feels broken

- **Stuck** in a half-typed command or wrong mode: `<Esc>`. Bad edit: `u` (or
  `<leader>uu` for the undo tree).
- **No completion / formatting** for a language: run `dev-doctor --all`, then use `setup.sh` to install the server or
  formatter, `:LspInfo` to confirm it attached, `<leader>lR` to restart it.
- **General health:** `:checkhealth`. **Plugins out of sync:** `:Lazy` then `S` to sync.
- **Secrets showing** in a `.env` file: `<leader>uc` toggles masking (cloak).

---

## One-page cheat sheet

| Want to…              | Press                         |
| :-------------------- | :---------------------------- |
| Back to safety        | `<Esc>` (then `u` to undo)    |
| Browse commands       | `Space`, then wait            |
| Find file / project   | `<leader>ff` / `<leader>fp`   |
| Search text           | `<leader>fg`                  |
| File explorer         | `-` or `<leader>e`            |
| Jump anywhere on screen | `<leader><leader>`          |
| Go to definition      | `gd` (back: `<C-o>`)          |
| Hover docs            | `K`                           |
| Rename symbol         | `<leader>lr`                  |
| Code action           | `<leader>la`                  |
| Format buffer         | `<leader>cf` (auto on save)   |
| Build / test          | `<leader>ob` / `<leader>on`   |
| Run Python file/tests | `<leader>op` / `<leader>oT`   |
| Git UI                | `<leader>gg`                  |
| Stage / preview hunk  | `<leader>gs` / `<leader>gp`   |
| Diagnostics list      | `<leader>xx`                  |
| Scratch buffer        | `<leader>.`                   |
| Search keymaps        | `<leader>fk`                  |

---

## Where to go next

- **[Neovim Development Workflow](nvim-dev-workflow.md)** — the deeper motion, search, code, task, and review loop.
- **[Neovim Research Workflow](nvim-research-workflow.md)** — Python, Markdown, and notebook tooling.
- **[Neovim Note-Taking Workflow](nvim-note-taking-workflow.md)** — a lightweight notes system in-editor.
- **[Neovim Starting Projects](nvim-starting-projects.md)** — repeatable project bootstrap routines.
- **[Neovim Setup Overview](nvim.md)** — the full map and plugin inventory.
