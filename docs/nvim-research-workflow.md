# Neovim Research Workflow

This guide explains how to use the current Neovim setup for research-style work without turning Neovim into a notebook UI.

Use Neovim for:

- Python modules and scripts
- Markdown notes and experiment logs
- SQL snippets and data queries
- pytest-backed checks
- project tasks (`<leader>ob`, `<leader>on`, `<leader>op`, `<leader>oT`)

Use external tools such as JupyterLab when you need rich notebook output, plots, or long interactive sessions.

---

## 1. Project Shape

A useful research repo layout:

```text
project/
  notes/
    2026-04-02-experiment-log.md
    reading-summary.md
  src/
    baseline.py
    analysis.py
  scripts/
    inspect_data.py
  sql/
    sample_query.sql
  tests/
    test_baseline.py
```

This gives Neovim clear roots for LSP, formatting, tests, and search.

---

## 2. Python Setup

Prefer `uv` projects:

```sh
uv init
uv add pytest ruff
mkdir -p src scripts tests notes sql
```

Python support in Neovim:

- Ruff provides linting, fixes, and formatting (format on save).
- basedpyright provides hover and type intelligence; `:PythonEnv` shows the interpreter it uses.
- `<localleader>r` opens an ipython REPL; `<localleader>e` sends the line or selection.
- `<leader>op` runs the current Python file.
- `<leader>oT` runs pytest for the project or current file context.

---

## 3. Notes and Search

Use Markdown for durable reasoning:

- experiment logs
- paper summaries
- decisions and assumptions
- links to scripts, queries, and datasets

Useful keys:

| Key | Action |
| :--- | :--- |
| `<leader>ff` | Find files |
| `<leader>fg` | Search notes and code |
| `<leader>f/` | Search current note |
| `<leader>fr` | Reopen recent files |
| `<leader>fs` | Jump to symbols in code |

Markdown gets treesitter highlighting; there is no in-editor rendering, so keep notes plain and readable as text.

---

## 4. Execution Loop

Use files as the unit of durable work:

1. Write or edit a script in `scripts/` or code in `src/`.
2. Run it with `<leader>op`.
3. Move reusable pieces into modules.
4. Add pytest checks in `tests/`.
5. Run checks with `<leader>oT` or `<leader>on`.
6. Record results in Markdown.

Use JupyterLab outside Neovim for exploratory visualization-heavy sessions, then move stable code back into normal Python files.

---

## 5. SQL and Data Notes

SQL files get treesitter highlighting and `sql-formatter` on save (conform). Keep reusable queries in `sql/`; `<leader>cf` formats on demand.

For data projects, keep commands explicit in notes:

```md
Input: data/raw/events.parquet
Script: scripts/inspect_data.py
Command: uv run python scripts/inspect_data.py
Result: notes/2026-04-02-experiment-log.md
```

This keeps experiments reproducible without depending on editor-local notebook state.
