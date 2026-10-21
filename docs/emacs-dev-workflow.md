# Emacs Development Workflow

> **Note:** this guide describes the previous, larger Emacs config (removed 2026-10-21). The current setup is in `emacs.md`; the old config is in git history.

This document is a practical guide to using the Emacs setup in this dotfiles repository as an actual daily software development environment.

It is not a generic Emacs manual. It is a walkthrough of how this specific config is meant to be used.

This guide uses Python as the example language, but the workflow applies more broadly to the rest of the programming setup.

If you are setting this up on a new machine, run `C-c e h` first. It checks the external tools this config expects and is the fastest way to spot missing language servers or local override files.

---

## 1. What this Emacs setup gives you for programming

This config is designed around a few core ideas:

- `project.el` is the center of project navigation.
- `consult` is the center of search and movement.
- `eglot` is the LSP client for code intelligence.
- `apheleia` handles formatting on save when formatters are installed.
- `vterm` gives you a real terminal inside Emacs.
- `magit` is the Git interface.

The most important prefixes to remember are:

| Key | Meaning |
| :--- | :--- |
| `C-c p` | Project commands |
| `C-c s` | Search commands |
| `C-c l` | LSP / diagnostics commands |
| `C-x g` | Git via Magit |
| `C-x b` | Switch buffers |
| `M-.` | Jump to definition / context action depending on mode |
| `M-,` | Jump back from definition |

The most useful search keys are:

| Key | Action |
| :--- | :--- |
| `C-c s s` | Search in current project |
| `C-c s r` | Ripgrep broadly |
| `C-c s l` | Search current buffer |
| `C-c s i` | Jump to symbol in current buffer |
| `C-c s o` | Jump by outline/headings |

---

## 2. Create a new Python project

For example, create a project directory:

```sh
mkdir -p ~/projects/demo-python-app
cd ~/projects/demo-python-app
python3 -m venv .venv
source .venv/bin/activate
python -m pip install pytest ruff
touch pyproject.toml main.py
mkdir -p tests
touch tests/test_main.py
git init
```

A small `pyproject.toml` might look like:

```toml
[project]
name = "demo-python-app"
version = "0.1.0"
requires-python = ">=3.11"

[tool.ruff]
line-length = 88
```

And a tiny `main.py`:

```python
def add(a: int, b: int) -> int:
    return a + b


if __name__ == "__main__":
    print(add(2, 3))
```

And `tests/test_main.py`:

```python
from main import add


def test_add() -> None:
    assert add(2, 3) == 5
```

At this point, you already have enough structure for Emacs to treat it as a project.

In this config, Python buffers then behave like this:

- `M-x compile` runs the current file with the project interpreter when possible
- `C-c p t` runs pytest for the current file when you are visiting one
- if `./.venv/bin/python` exists, Emacs prefers it automatically
- if `basedpyright-langserver` is installed, Eglot uses it for Python language intelligence
- `ruff` handles linting and formatting; Apheleia runs it on save

---

## 3. Open the project in Emacs

Start Emacs and enter the project in one of these ways:

- `C-c p P` to switch to a known project and immediately pick a file
- `C-c p p` to switch to a known project
- `C-x C-f` and enter `~/projects/demo-python-app/main.py`
- or open any file under the project

If this is a brand-new non-Git directory and you want it to appear in project switching later:

- visit the directory or a file inside it
- run `C-c p a` to remember it as a known project

Once you are inside a project, `project.el` understands the root automatically using markers such as:

- `pyproject.toml`
- `Cargo.toml`
- `compile_commands.json`
- `.envrc`
- Git metadata

That means all project commands now operate relative to the project root.

---

## 4. Basic editing flow

Open files with:

- `C-x C-f` for a direct path
- `C-c p f` for project file search
- `C-x b` to switch buffers

This setup uses:

- `vertico` for a compact completion UI
- `orderless` for flexible matching
- `marginalia` for minibuffer annotations
- `consult-buffer` for buffer switching

### How to think about file opening

Use different commands for different intent:

- You know the exact path: `C-x C-f`
- You know the file is in this project: `C-c p f`
- You were just in it recently: `C-x b`
- You want to search by content rather than filename: `C-c s s`

Strong Emacs use is less about one universal “open” action and more about choosing the shortest path to the target.

---

## 5. Add new files

You usually add files with `C-x C-f`.

For example:

1. Press `C-x C-f`
2. Type `app/models/user.py`
3. Press `RET`
4. Emacs will ask to create missing directories and the new file
5. Start editing
6. Save with `C-x C-s`

That is the normal way to create files in Emacs. You do not need a tree view for it.

If you prefer seeing the filesystem:

- `C-c p D` opens Dired at the project root
- `C-x d` opens Dired for any directory

But for most work, file creation via `C-x C-f` is faster than navigating a tree.

---

## 6. Move around the project quickly

This config is optimized for keyboard-first navigation.

### Move between files

- `C-c p f` finds a file inside the project
- `C-x b` switches between open buffers
- `C-c p b` shows project buffers
- `C-c p P` jumps into a different known project and opens a file there

### Move inside one file

- `C-c s i` uses `consult-imenu` to jump to classes, functions, methods, headings, and other symbols
- `M-g g` goes to a line number
- `C-c s l` searches within the current buffer
- `M-g o` jumps by outline heading

### Move by symbol intelligence

When `eglot` is active:

- `C-c l d` goes to definition
- `C-c l D` finds references
- `C-c l i` finds implementations
- `C-c l t` finds type definitions
- `M-,` jumps back after a definition jump

This is the normal “read code” loop:

1. Use `C-c s i` to jump to a local symbol.
2. Use `C-c l d` to jump into definitions elsewhere.
3. Use `M-,` to pop back.
4. Use `C-x b` to move across already-open files.

### Language-specific defaults worth knowing

- Python: current buffer compile command runs the file; project tests use pytest; local `.venv` is preferred automatically
- Rust: compile defaults to `cargo check`; project tests use `cargo test`
- C/C++: compile defaults to `cmake --build build`; project tests run build plus `ctest`
- OCaml: compile defaults to `dune build`; project tests use `dune test`; `ocamllsp` is preferred over Merlin when available

---

## 7. Search like a working developer

This config expects you to use search constantly.

### Search in the current file

Use:

- `C-c s l`

This is good for:

- finding a function in a long file
- locating a string
- moving around a config file quickly

### Search the whole project

Use:

- `C-c s s`

This runs `consult-ripgrep` at the project root.

Use it for:

- finding where a function is called
- locating a config variable
- finding all TODOs
- looking for a class name or error message

Typical examples:

- search `add(`
- search `pytest`
- search `TODO`
- search `user_id`

### Search broadly

Use:

- `C-c s r`

This is the general ripgrep entry point when you are not thinking strictly in terms of project-root flow.

### Edit grep results

The config includes `wgrep`, so when you work from grep-style buffers, you can turn them into editable results and apply changes across matches.

That is useful for bulk refactors such as:

- rename a repeated string
- change logging format
- edit repeated import patterns

---

## 8. Dired and the project finder

These two tools have different jobs in this setup.

### Use `C-c p f` as the default file finder

This is the everyday command.

Use it when:

- you know the file is in the current project
- you want fast keyboard navigation
- you do not need to look at the directory tree

### Use `C-c p D` or `C-x d` for filesystem browsing

Use Dired when:

- you want to rename, move, copy, or delete files
- you want to inspect a directory directly
- you want the built-in file explorer

The distinction is:

- `C-c p D` is current-project Dired
- `C-x d` is arbitrary-directory Dired

---

## 8. Use Python support in this setup

Python files use:

- `python-ts-mode` when tree-sitter is available
- `python-mode` otherwise
- `eglot-python-preset`
- formatting through `apheleia`

The setup also assigns a sensible local test command for the current Python buffer.

### What that means in practice

Open `main.py`. You should expect:

- syntax highlighting
- minibuffer and popup completion
- LSP navigation if the environment and tools are available
- buffer-local compile/test behavior
- formatting on save if the formatter exists

### LSP workflow

In a Python buffer:

- `C-c l d` goes to definition
- `C-c l D` finds references
- `C-c l r` renames a symbol
- `C-c l a` shows code actions
- `C-c l f` formats the buffer
- `C-c l e` shows diagnostics for the current buffer
- `C-c l n` moves to next diagnostic
- `C-c l p` moves to previous diagnostic

### Completion workflow

This config gives you:

- `corfu` popup completion in the buffer
- `cape` file and dabbrev completion helpers

Typical flow:

1. Start typing a symbol or method.
2. Wait briefly for popup completion.
3. Use completion candidates.
4. If navigation is needed, jump with LSP keys instead of searching manually.

---

## 9. Run tests and build-like tasks

This setup gives you two important project commands:

- `C-c p m` for compile/build
- `C-c p t` for tests

### In Python

The config computes a Python-oriented command such as:

```sh
/path/to/project/.venv/bin/python -m pytest path/to/current_file.py
```

or falls back to:

```sh
python3 -m pytest
```

depending on context.

This means your normal loop becomes:

1. Edit code.
2. Save with `C-x C-s`.
3. Run tests with `C-c p t`.
4. Fix failures.
5. Repeat.

### Compile buffer behavior

Compilation output appears in a standard compilation buffer, so:

- errors are clickable
- next error navigation works
- Emacs can jump you to the failing file/line

This is one of the main productivity advantages of doing the workflow inside Emacs rather than bouncing between editor and shell for every small action.

---

## 10. Open a shell inside the project

Use:

- `C-c p v`

This opens `vterm` rooted at the current project.

If you started Emacs outside the project, `C-c p V` switches to a known project and opens a terminal there in one step.

This is useful for:

- running `pytest -k something`
- activating environments
- starting servers
- running `python main.py`
- inspecting Git state with normal shell tools

Typical loop:

1. Code in one window.
2. Run a project terminal with `C-c p v`.
3. Split the frame and keep test/server output visible.

---

## 11. Work with Git using Magit

Use:

- `C-x g`

This opens `magit-status`.

Typical flow:

1. Edit code.
2. Save files.
3. `C-x g`
4. Review unstaged changes.
5. Stage selected hunks or files.
6. Commit.
7. Push.

This config also includes `diff-hl`, which shows gutter markers for changed lines in programming buffers and Dired.

That gives you two levels of Git feedback:

- local line-level hints while editing
- full repo management in Magit

---

## 12. Typical Python day-to-day flow

Here is a realistic sequence using this setup.

### Start work

1. Open Emacs.
2. Open a project file with `C-c p P`.
3. Use `C-x b` to return to recently used files.

### Understand the current code

1. Use `C-c s s` to search for a class or function.
2. Use `C-c s i` to jump to symbols in the current buffer.
3. Use `C-c l d` to follow definitions.
4. Use `M-,` to return.

### Add a new feature

1. Create a new file with `C-x C-f`.
2. Add the implementation.
3. Open the related test file with `C-c p f`.
4. Add tests.
5. Save with `C-x C-s`.

### Run and debug

1. Run tests with `C-c p t`.
2. Read failures in the compilation buffer.
3. Jump to the failing location.
4. Use `C-c l e` and `C-c l n` for diagnostics.
5. Re-run tests.

### Commit

1. `C-x g`
2. Stage hunks
3. Write commit message
4. Push

That is the intended SWE loop for this Emacs setup.

---

## 13. Recommended habits for this config

If you want this setup to feel fast, build these habits:

- Use `C-c p f` instead of browsing file trees.
- Use `C-c s s` instead of manually scanning folders.
- Use `C-c s i` and LSP jumps instead of scrolling.
- Use `C-c p t` and `C-c p m` instead of repeatedly retyping shell commands.
- Use `C-x b` aggressively.
- Use Magit instead of dropping to raw Git for routine work.

The config is strongest when used as a navigation-and-command environment, not just as a text editor.

---

## Short Reference

| Key | Action |
| :--- | :--- |
| `C-c p f` | Find project file |
| `C-c p d` | Open project Dired |
| `C-c p v` | Open project terminal |
| `C-c p t` | Run project tests |
| `C-c p m` | Compile/build project |
| `C-c s s` | Search current project |
| `C-c s l` | Search current buffer |
| `C-c s i` | Jump to symbol in buffer |
| `C-c l d` | Go to definition |
| `C-c l D` | Find references |
| `C-c l r` | Rename symbol |
| `C-x g` | Magit status |

## Final advice

Use this Emacs setup as an environment, not just an editor.

That means:

- search instead of browse
- jump instead of scroll
- run project commands from inside Emacs
- keep code, search, diagnostics, and Git close together

That is where this configuration becomes much more valuable than a plain text editor.
