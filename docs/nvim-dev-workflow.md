# Neovim Development Workflow

The daily engineering loop with this config. The pieces:

- snacks picker for files, text, symbols and diagnostics
- oil for filesystem edits
- the built-in LSP client (servers from mason) for navigation and actions
- blink.cmp for completion, conform for format on save
- `core/tasks.lua` for builds, tests, watch mode and the REPL
- gitsigns, codediff and LazyGit for review

Every key here is also in [`nvim.md`](nvim.md).

---

## 1. Mental model

Think in four loops.

### Read

`<leader>ff` for files, `<leader>fg` (or `<leader>/`) for text,
`<leader>fs` for symbols in the file, `<leader>fS` across the workspace,
and `gd`/`grr`/`gri` once a server is attached. `]]`/`[[` jump between
uses of the word under the cursor.

### Edit

Buffers (`<leader>,`, `<leader>bn`/`bp`/`bd`), comments (`gcc`, `gc` in
Visual mode), auto-pairs, syntax selection (`an`/`in` in Visual mode),
completion with `<CR>` to accept, and `<C-s>` to save (which formats).

### Execute

| Key | Action |
| :--- | :--- |
| `<leader>ob` / `<leader>on` | Build / test the project |
| `<leader>os` / `<leader>od` | Pick a `package.json` script / run `dev` |
| `<leader>op` / `<leader>oT` | Run the current Python file / pytest |
| `<leader>oC` | Compile the current C/C++ file |
| `<localleader>w` / `W` | Rebuild / re-test on every save |
| `<localleader>r` | Toggle the REPL; `,e`/`,b`/`,f` send line, buffer, form |
| `<C-/>` | Terminal on the right |

### Review

`]h`/`[h` to move across hunks, `<leader>gp` to preview,
`<leader>gs`/`<leader>gr` to stage/reset, `<leader>gv` for the side-by-side
diff view, `<leader>gg` for LazyGit.

---

## 2. Open a project

Start from the repository root with `nvim` (or `nvim .`, which opens oil).

1. `<leader>ff` to pick an entry file; `<leader>fr` for recent ones.
2. `<leader>fg` to find an important symbol or concept.
3. `gd`, `grr` and `gri` to navigate semantically; `<C-o>` back.
4. `<leader>,` to switch between the files you have open.
5. `<leader><tab>s` saves the session for this directory;
   `<leader><tab>o` restores it next time.

---

## 3. Code intelligence

| Key | Action |
| :--- | :--- |
| `gd` / `gD` | Definition / declaration |
| `grr` / `gri` / `grt` | References / implementation / type definition |
| `K` | Hover docs |
| `<leader>la` | Code action |
| `<leader>lr` | Rename |
| `<leader>ld` | Line diagnostics |
| `<leader>lh` | Toggle inlay hints |
| `<leader>lc` | Run code lens |
| `<leader>lR` | Restart attached servers |
| `]d` / `[d` | Next / previous diagnostic |
| `<leader>xx` / `<leader>xb` | Diagnostics, all buffers / this buffer |

`<leader>ci`, `cI`, `cu`, `cF` and `cE` organize imports, add missing ones,
remove unused ones, fix all, and apply ESLint fixes. Python uses ruff for
lint/fix/format and basedpyright for types; Go uses gopls; Rust
rust-analyzer.

---

## 4. Formatting

| Key | Action |
| :--- | :--- |
| `<leader>cf` | Format the buffer (or selection) |
| `:ToggleFormatter` | Format on save off/on for this buffer |
| `:ConformInfo` | Which formatter the buffer uses |

Format on save is on, with a 1000ms timeout. Web files (JS/TS/JSX/TSX,
JSON, CSS/SCSS/Less, HTML, Markdown/MDX, YAML) use the first installed of
oxfmt, prettierd, prettier. Lua uses stylua, sh/bash shfmt, fish
fish_indent, SQL sql-formatter. Everything else falls back to its language
server; Go organizes imports first. Linting comes from the language servers
(ruff, eslint, oxlint) as diagnostics; there is no separate linter.

---

## 5. Project tasks

`<leader>ob`/`<leader>on` pick commands from the nearest root marker:

| Project | Build / test |
| :--- | :--- |
| Node | `package.json` scripts (npm/pnpm/yarn/bun by lockfile) |
| Python | `uv run` when `uv.lock`/`pyproject.toml` exists; pytest for tests |
| Go | `go build ./...` / `go test ./...` |
| Rust | `cargo build` / `cargo test` |
| C/C++ | CMake, Meson or Make / `make test` |
| OCaml | `dune build` / `dune runtest` |
| Haskell | Cabal or Stack |
| Java | Maven or Gradle (wrappers preferred) |
| Racket | `raco make` / `raco test` |

Commands run in a snacks terminal. Debugging (Go, through Delve): `<F5>`
starts, `<leader>db` sets a breakpoint, `<leader>dt` debugs the test under
the cursor.

---

## 6. Git review

| Key | Action |
| :--- | :--- |
| `]h` / `[h` | Next / previous hunk |
| `<leader>gp` | Preview hunk |
| `<leader>gs` / `<leader>gr` | Stage / reset hunk (or selected lines) |
| `<leader>gS` / `<leader>gR` | Stage / reset buffer |
| `<leader>gh` | Picker of all hunks |
| `<leader>gb` / `<leader>ga` / `<leader>ub` | Blame line / file / toggle end-of-line blame |
| `<leader>gv` | Diff view: changed-files tree + side-by-side diffs |
| `<leader>gl` / `<leader>gL` | History / this file's history |
| `<leader>gg` | LazyGit |
| `<leader>gB` / `gi` / `gP` | Open on GitHub / issues / pull requests |

In the diff view, moving through the explorer opens each file's diff; `-`
stages a file, `]c`/`[c` jump between hunks, `]f`/`[f` between files, and
`gc` folds unchanged code down to 8 lines of context.

The intended flow: edit, save (formats), run the relevant task, then review
and stage hunks continuously.

---

## 7. Daily flow

1. Open the repo with `nvim`; `<leader><tab>o` to restore the session.
2. Jump to an entry file with `<leader>ff`.
3. Search the task with `<leader>fg`.
4. Navigate with `gd`, `grr` and `gri`.
5. Edit and save with `<C-s>` (formats on save).
6. Run project work with `<leader>ob`, `<leader>on`, `<leader>os`,
   `<leader>op` or `<leader>oT`.
7. Review with `<leader>gv`, stage with `-` or `<leader>gs`, commit in
   `<leader>gg`.
