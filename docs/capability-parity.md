# Emacs and Neovim capability contract

> **Note (2026-10-21):** Emacs no longer aims for parity. It is now a
> near-stock setup for Haskell, OCaml and notes only (`emacs.md`); Neovim is
> the full environment. Rows below that describe other Emacs features refer
> to the previous config.

The editors share external binaries and workflows while keeping native interaction styles. Emacs uses built-in projects, Eglot, Apheleia, Dape, Magit, Denote and Citar. Neovim uses the built-in LSP client (servers from mason), blink.cmp, conform, nvim-dap, gitsigns/codediff and snacks.

## Tool ownership

Tools resolve in this order: project environment (`direnv`, `.venv`, `node_modules/.bin`, opam switch), `~/.local/bin`, then the system PATH. Neovim's mason installs its own language servers, formatters and debugpy (its bin directory goes first on Neovim's PATH); clangd, ocamllsp, hls and jdtls come from their toolchains so they match the compiler. Run `dev-doctor --all` from a terminal, or `:checkhealth` in Neovim.

`setup.sh` installs tools; `setup.sh --check` is read-only. Editor plugins remain pinned by their package metadata or Neovim's `nvim-pack-lock.json`. Projects own compiler and dependency versions.

## Language matrix

The Neovim column below is current; Emacs covers only OCaml and Haskell
(same servers and formatters).

| Language | LSP (Neovim) | Formatter (Neovim) | Build/test |
| :--- | :--- | :--- | :--- |
| TypeScript/JavaScript/React | vtsls + oxlint (+ ESLint with a config) | oxfmt / prettierd / Prettier, first installed | package-manager scripts |
| Python | basedpyright + Ruff | Ruff | uv/pytest |
| Rust | rust-analyzer | rustfmt | Cargo |
| Go | gopls | gofumpt via gopls, imports organized | go build / go test |
| C/C++ | clangd | clang-format | Make/CMake/Meson |
| OCaml | ocamllsp | ocamlformat | dune/utop |
| Haskell | HLS | ormolu | Cabal/Stack |
| Racket | — | — | raco make / raco test |
| Standard ML | — | — | SML/NJ REPL |
| Java | jdtls (Homebrew) | Eclipse formatter via jdtls | Maven/Gradle, JShell |
| Lua | lua-language-server | stylua | project command |
| shell | bashls + shellcheck | shfmt | project command |
| fish | — | fish_indent | project command |
| CSS/HTML | cssls, tailwindcss | oxfmt / prettierd / Prettier | project command |
| JSON/YAML | jsonls / yamlls with SchemaStore schemas | oxfmt / prettierd / Prettier | project command |
| Markdown | marksman | oxfmt / prettierd / Prettier | project command |
| TOML | taplo | taplo | project command |
| SQL | none | sql-formatter | interactive client |

Missing optional binaries leave editing usable and are reported by the doctor.

## Shared workflows

| Capability | Emacs | Neovim |
| :--- | :--- | :--- |
| Project search | `C-c s s` | `<leader>fg` |
| Build/test | `C-c p m` / `C-c p t` | `<leader>ob` / `<leader>on` |
| LSP action/rename | `C-c l a` / `C-c l r` | `<leader>la` / `<leader>lr` |
| Format | `C-c l f` or save | `<leader>cf` or save |
| Debug | `C-c d` prefix | `<leader>d` prefix (Go: Delve, Python: debugpy) |
| HTTP request | Restclient `.http` buffer | — |
| Terminal agent | `C-c g g` | `<leader>aa` |
| Agent context | `C-c g c` | `<leader>ac` |
| File browser | `SPC o` (dirvish-side) | `-` (oil float) |
| Terminal toggle (right) | `SPC '` / `C-c '` | `<C-/>` |
| Tabs / workspaces | `SPC TAB …`, `gt` | `<leader><tab>…`, `gt` |
| Sessions | desktop-save / `SPC TAB` | `<leader><tab>s` / `<leader><tab>o` (per directory) |
| REPL (OCaml, Haskell, Racket, SML, Python, Java) | `C-c f o/h/r/m/p` opens; fp-repl `C-c C-v` evaluates in place | `<localleader>r` toggles; `<localleader>e` / `b` / `f` send line or selection, buffer, form |
| Notebooks (Jupyter) | code-cells + jupytext | — |
| Breadcrumbs | breadcrumb (header line) | — |
| Fuzzy finder | Vertico + Consult + Orderless | snacks.picker |
| Snippets | tempel (`<name`) | LSP snippets through blink.cmp |
| Inline AI suggestions | — | Copilot through `vim.lsp.inline_completion` (`:LspCopilotSignIn`) |
| Text objects / surround | evil + evil-surround | mini.ai (`af`, `ac`, `aa`…) + mini.surround (`gsa`/`gsd`/`gsr`) |
| Side-by-side diff / history | built-in `vc` (`C-x v =`, `C-x v l`) | codediff (`<leader>gv` / `<leader>gl`) |

## Shared look

Emacs is deliberately near-stock: the default theme and mode line, with
only evil, vertico/consult, Haskell/OCaml tooling and Denote + Org notes on
top (see `emacs.md`). Neovim picks its theme family with `<leader>uC`
(light/dark follows theme-mode unless pinned). Ghostty, tmux, starship and
btop use Kanagawa; text is **Source Code Pro** (SauceCodePro Nerd Font) with
Symbols Nerd Font Mono for icons.

Set `DEV_AGENT` to a command name to override agent selection. Otherwise both editors choose `codex`, then `claude`. Context commands copy an explicit `@file:start-end` reference and open a project-root terminal; paste intentionally rather than sending data silently.

## Portable research data

Durable notes are Markdown files under `${NOTES_DIR:-~/notes}` with Denote-compatible timestamp identifiers and YAML front matter. The bibliography defaults to `${BIBLIOGRAPHY:-~/notes/references.bib}` and citations use Pandoc syntax (`[@key]`). PDF locations belong in BibTeX `file` fields.

| Action | Emacs | Neovim |
| :--- | :--- | :--- |
| New/find/search note | `C-c n n/f/s` | — (edit as Markdown; `<leader>ff`/`fg` in `~/notes`) |
| Link/backlinks | `C-c n l/b` | — |
| Browse citations | `C-c n c` | — |
| Insert Pandoc citation | `C-c n C` | — |

Org capture and agenda remain an Emacs-only task-management enhancement. They are not the durable note format.
