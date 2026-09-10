# Neovim Development Workflow

This guide describes the daily engineering loop for the current Neovim config in this repository.

The setup is centered on a few tools:

- Telescope for files, text, diagnostics, and symbols.
- Oil for filesystem edits.
- Native LSP for code navigation and actions.
- Conform and nvim-lint for cleanup.
- Overseer for scripts, builds, tests, and one-off commands.
- Gitsigns, LazyGit, and Diffview for review.

---

## 1. Mental Model

Think in four loops.

### Read

Use `<leader>ff` or `<leader>fp` for files, `<leader>fg` for text, `<leader>fs` for document symbols, and `gd`/`grr`/`gri` once LSP is attached.

### Edit

Use buffer keys (`<leader>bn`, `<leader>bp`, `<leader>bd`), comments (`gcc`, visual `gc`/`gb`), surround, Flash, and `<C-s>` to keep edits quick.

### Execute

Use Overseer for all execution:

| Key | Action |
| :--- | :--- |
| `<leader>or` | Run any task/template |
| `<leader>os` | Pick a `package.json` script |
| `<leader>od` / `<leader>ol` / `<leader>oy` / `<leader>of` | Run `dev`, `lint`, `typecheck`, or `format` package scripts |
| `<leader>op` | Run current Python file |
| `<leader>oT` | Run Python tests with pytest |
| `<leader>ob` | Build current project |
| `<leader>on` | Run current project tests |
| `<leader>oC` | Compile current C/C++ file |

### Review

Use `]h`/`[h` to move across hunks, `<leader>gp` to preview, `<leader>gs`/`<leader>gr` to stage/reset, and `<leader>gg` for LazyGit.

---

## 2. Open a Project

Start from the repository root with `nvim`.

1. Use `<leader>fp` to pick a likely entry file.
2. Use `<leader>fg` to find an important symbol or concept.
3. Use `gd`, `grr`, and `gri` to navigate semantically.
4. Use `<leader>fb` and `<leader>fr` to return to active or recent files.

This config is search-first; there is no separate quick-file bookmarking layer.

---

## 3. Code Intelligence

Core LSP keys:

| Key | Action |
| :--- | :--- |
| `gd` / `gD` | Definition / declaration |
| `grr` / `gri` / `grt` | References / implementation / type definition (Neovim defaults) |
| `K` | Hover docs (Neovim default) |
| `<leader>la` | Code action |
| `<leader>lr` | Rename with live preview |
| `<leader>ld` | Line diagnostics |
| `<leader>lh` | Toggle inlay hints (buffer); `<leader>ui` toggles globally |
| `<leader>lR` | Restart attached LSP clients |
| `]d` / `[d` | Next / previous diagnostic (float opens on arrival) |

For JS/TS, `<leader>ci`, `<leader>cI`, `<leader>cu`, `<leader>cF`, and `<leader>cE` cover import organization and fix-all actions. Python uses Ruff for lint/fix/format and basedpyright for hover/type intelligence.

---

## 4. Formatting and Linting

| Key | Action |
| :--- | :--- |
| `<leader>cf` | Format buffer or selection |
| `<leader>ll` | Trigger linting |

Format on save is enabled. Prettier handles JS/TS/Vue/JSON/YAML/Markdown/HTML/CSS, Ruff handles Python, and `sql-formatter` handles SQL.

---

## 5. Project Tasks

Overseer chooses commands from common root markers:

| Project | Build/Test behavior |
| :--- | :--- |
| Node | `package.json` scripts |
| Python | `uv run python` when `uv.lock`/`pyproject.toml` exists; pytest for tests |
| C/C++ | CMake, Meson, or Make |
| Rust | `cargo build`, `cargo test` |
| OCaml | `dune build`, `dune runtest` |
| Haskell | Cabal or Stack |

Use `<leader>ot` for the task panel and `<leader>oa` for task actions.

---

## 6. Git Review

Useful keys:

| Key | Action |
| :--- | :--- |
| `]h` / `[h` | Next / previous hunk |
| `<leader>gp` | Preview hunk |
| `<leader>gs` / `<leader>gr` | Stage / reset hunk |
| `<leader>gS` / `<leader>gR` | Stage / reset buffer |
| `<leader>gg` | LazyGit |
| `<leader>go` / `<leader>gc` / `<leader>gh` | Diffview open / close / file history |

The intended flow is edit, format, run the relevant task, then review and stage hunks continuously.

---

## 7. Daily Flow

1. Open the repo with `nvim`.
2. Jump to an entry file with `<leader>fp`.
3. Search the task with `<leader>fg`.
4. Navigate with `gd`, `grr`, and `gri`.
5. Edit and save with `<C-s>`.
6. Format or lint with `<leader>cf` and `<leader>ll` if needed.
7. Run project work with `<leader>ob`, `<leader>on`, `<leader>os`, `<leader>op`, or `<leader>oT`.
8. Review changes with Gitsigns and LazyGit.
