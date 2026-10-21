# Neovim Starting Projects

> **Note:** this guide predates the near-stock Neovim setup (2026-10-21); plugins it mentions may be gone. The current setup is in `nvim.md`.

This guide explains how to start a new project so that this Neovim setup becomes productive immediately.

The important principle is simple:

- create a project root with the right markers
- open it in Neovim
- let LSP, tasks, tests, and formatting attach to that structure

Most of the productivity gains come from project shape, not from editor tricks.

---

## 1. What the editor is looking for

This config detects project roots from normal build and language markers.

Examples:

- JavaScript and TypeScript: `package.json`, `tsconfig.json`, `jsconfig.json`
- Python: `pyproject.toml`, `uv.lock`, `pytest.ini`, `.venv`
- C and C++: `CMakeLists.txt`, `Makefile`, `meson.build`, `compile_commands.json`
- OCaml: `dune-project`, `dune-workspace`, `dune`, `*.opam`
- Haskell: `cabal.project`, `*.cabal`, `stack.yaml`, `package.yaml`

If you create the right markers early, the editor behaves correctly much sooner.

---

## 2. General bootstrap routine

For any new project:

1. Make a root directory.
2. Add the tool-specific project marker files.
3. Initialize Git.
4. Open the root with `nvim`.
5. Use `<leader>fp` or `<leader>ff` to open the first real file.

That is enough to activate most of the value in this config.

---

## 3. Start a Python project

Example:

```sh
mkdir -p ~/projects/demo-python
cd ~/projects/demo-python
uv init
uv add pytest ruff
mkdir -p src tests notes
touch src/main.py tests/test_main.py notes/index.md
git init
```

Why this works well here:

- Python LSP attaches from the project root
- Overseer prefers `uv run`
- `<leader>op` runs the current file
- `<leader>oT` runs tests
- Ruff handles lint/format actions and Ty handles type intelligence

---

## 4. Start a JS or TS project

Example:

```sh
mkdir -p ~/projects/demo-ts
cd ~/projects/demo-ts
npm init -y
npm install -D typescript eslint prettier vitest
npx tsc --init
mkdir -p src tests notes
touch src/index.ts tests/index.test.ts notes/index.md
git init
```

Why this works well here:

- `vtsls` attaches from `package.json` or `tsconfig.json`
- `eslint` attaches when the project has an ESLint config
- formatting uses Prettier
- `<leader>ci`, `<leader>cI`, `<leader>cu`, and `<leader>cF` are useful immediately
- `<leader>os` runs package scripts
- `<leader>os` can run project test scripts such as `test`, `vitest`, or `jest`

---

## 5. Start a C or C++ project

Example CMake layout:

```sh
mkdir -p ~/projects/demo-cpp/src ~/projects/demo-cpp/tests
cd ~/projects/demo-cpp
touch CMakeLists.txt src/main.cpp tests/test_main.cpp
mkdir build
git init
```

Why this matters:

- `clangd` finds the root from `CMakeLists.txt`
- `<leader>ob` builds the project
- `<leader>on` runs project tests when the build directory is ready
- `<leader>oC` compiles the current file into a local `.out`

If you use CMake seriously, generate `compile_commands.json` early. That improves `clangd` behavior.

---

## 6. Start an OCaml project

```sh
mkdir -p ~/projects/demo-ocaml/lib ~/projects/demo-ocaml/test
cd ~/projects/demo-ocaml
dune init proj demo_ocaml
git init
```

This setup then gives you:

- `ocamllsp`
- `dune build` on `<leader>ob`
- `dune runtest` on `<leader>on`

---

## 7. Start a Haskell project

Example Cabal layout:

```sh
mkdir -p ~/projects/demo-haskell/app ~/projects/demo-haskell/src ~/projects/demo-haskell/test
cd ~/projects/demo-haskell
cabal init
git init
```

This setup then gives you:

- `hls`
- `ormolu`
- `cabal build` on `<leader>ob`
- `cabal test` on `<leader>on`

If you prefer Stack, add `stack.yaml` early so root detection and task selection are unambiguous.

---

## 8. Create the project note structure on day one

Even for code-heavy projects, start with:

```text
notes/
  index.md
  decisions.md
  todo.md
```

That works well with this Neovim setup because:

- Markdown rendering is already configured
- Telescope grep searches notes and code together
- Recent files and buffer switching keep active notes easy to reopen

---

## 9. First ten minutes in a fresh project

A strong first-session routine:

1. Open the root in Neovim.
2. Use `<leader>fp` or `<leader>ff` to open the first source file.
3. Create `notes/index.md`.
4. Use `<leader>fg` to search for the main entrypoint or TODO markers.
5. Use `<leader>fb` and `<leader>fr` to move between active files and recent notes.
6. Run the relevant build or script command once.
7. Confirm LSP is attached and navigation works.

If you do that much, the project is usually in good shape for the next session.

---

## 10. The real rule

Starting projects well in this setup is mostly about one discipline:

make the project explicit early.

That means:

- choose the build system
- create the root markers
- add tests early
- add notes early
- let the editor attach to real structure from the start
