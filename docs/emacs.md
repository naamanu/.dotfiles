# Emacs Setup

Near-stock Emacs 31: the default theme, mode line and keybindings, with only
what this work needs on top. Everything lives in `~/.emacs.d/init.el`.

## What is added

| Area | Packages |
| :--- | :--- |
| Vim keys | evil, evil-collection, evil-surround, evil-commentary, evil-org |
| Finding things | vertico, orderless, marginalia, consult; built-in completion-preview |
| Haskell | haskell-mode (+ interactive-haskell-mode / GHCi), eglot + hls (ormolu on save) |
| OCaml | tuareg, eglot + ocamllsp (ocamlformat on save), ocaml-eglot, utop, dune; home-grown fp-repl, dune-transient, ocaml-spec-kit |
| Notes | denote, denote-journal, denote-markdown, consult-denote, markdown-mode; Org agenda + capture |

## Keys

| Key | Action |
| :--- | :--- |
| `SPC SPC` / `SPC .` / `SPC ,` / `SPC b` / `SPC /` | M-x / find file / recent file / buffer / ripgrep |
| `SPC p` | project.el prefix (stock `C-x p`) |
| `C-c n n` / `j` | New Denote note / today's journal |
| `C-c n f` / `s` | Find note / grep notes |
| `C-c n l` / `b` / `r` | Insert link / backlinks / rename from front matter |
| `C-c a` / `C-c c` (also `SPC a` / `SPC c`) | Org agenda / capture |
| `C-c C-l`, `C-c C-z` (Haskell) | Load into GHCi / switch to GHCi |
| `C-c C-e`, `C-c C-b` (OCaml) | utop: eval phrase / buffer |
| `C-c b` (OCaml) | dune menu (dune-transient) |
| `C-c i i` / `d` / `w` / `q` (OCaml) | mli-lens: show / diff / write / quit |
| `C-c i s` (OCaml) | odoc spec comment for a `val` |
| `M-g i`, `M-g f`, `M-s l` | consult imenu / flymake / line |

Everything else is stock Emacs; `C-h k` describes any key.

## Notes are shared with Neovim

`~/notes` is a Denote directory (`denote-file-type 'markdown-yaml`). Neovim's
`<leader>n` keys write the exact same file names, front matter and
`[title](denote:ID)` links, so either editor works on any note.
