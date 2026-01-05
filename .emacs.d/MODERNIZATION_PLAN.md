# Emacs Configuration Modernization Plan

## Aligning with Neovim Setup

Based on comparing your Neovim and Emacs configurations, here are the key differences and recommendations to make them more consistent.

---

## 🎯 Key Differences

### 1. **Color Scheme & Theme**

- **Neovim**: Using `tokyonight` (moon variant) with full transparency
- **Emacs**: Using `doom-one` theme without transparency
- **Gap**: Different aesthetic, lack of transparency support

### 2. **LSP Support Coverage**

- **Neovim**: Comprehensive LSP support for 20+ languages including:

  - Functional languages: OCaml, Haskell, Elm, Gleam, PureScript, Clojure, Racket, Elixir, Erlang, Scala, Nix
  - Systems: Rust, Go, C/C++
  - Web: TypeScript/JavaScript, JSON, YAML
  - Others: Python (Ruff), Bash, Lua

- **Emacs**: LSP configured via Eglot with similar coverage but:
  - Missing some functional language LSPs (Gleam, PureScript, Clojure, Elixir, Erlang, Scala, Nix, Racket)
  - Less configuration for specific language settings

### 3. **Treesitter Support**

- **Neovim**: Extensive treesitter with 30+ parsers installed
  - Includes all functional languages, BEAM ecosystem, Lisp family
  - Advanced treesitter features: context, autotag, textobjects
- **Emacs**: Basic `treesit-auto` with limited parsers
  - Only 8 parsers configured (rust, go, haskell, typescript, tsx, javascript, c, cpp)
  - Missing: OCaml, Elm, Gleam, PureScript, Clojure, Racket, Scheme, Erlang, Elixir, Scala, Nix

### 4. **Modern Features & Plugins**

- **Neovim has** (Emacs missing):

  - Advanced navigation: `flash.nvim`, `harpoon`
  - Smart session management: `auto-session`
  - Modern UI: `noice.nvim`, `dressing.nvim`, `notify`
  - Git integration: `gitsigns`, `diffview`, `git-blame`
  - Smart indentation detection
  - Debugging support (DAP)
  - Better formatting/linting integration
  - Modern completion with `nvim-cmp`

- **Emacs has** (Neovim missing):
  - `projectile` for project management
  - `embark` for context actions
  - Built-in Org mode
  - `vterm` integration

### 5. **Keybindings Philosophy**

- **Neovim**: Space as leader, comma as local leader

  - Modern, ergonomic bindings
  - Well-documented with `which-key`
  - Consistent prefixing scheme

- **Emacs**: Traditional Emacs bindings (with Evil mode)
  - Less consistent leader key usage
  - Missing `which-key` equivalent for discovery

### 6. **Core Settings Alignment**

#### ✅ Similar:

- Line numbers (relative)
- Tab width (2 spaces)
- Mouse support
- Auto-revert
- Recent files

#### ❌ Different:

- **Scrolloff**: Neovim has `scrolloff=8`, Emacs doesn't
- **Clipboard**: Neovim appends to system clipboard, Emacs doesn't explicitly
- **Color column**: Neovim has at 100, Emacs missing
- **Cursor line**: Both have it
- **Undo history**: Neovim persists to file, Emacs doesn't
- **Transparency**: Neovim fully transparent, Emacs opaque

---

## 📋 Recommended Changes

### High Priority

#### 1. **Add Transparency Support**

Match Neovim's transparent background:

```elisp
;; In el-theme.el
(set-face-attribute 'default nil :background "unspecified-bg")
(set-face-attribute 'fringe nil :background "unspecified-bg")

;; For terminal Emacs
(unless (display-graphic-p)
  (set-face-background 'default "unspecified-bg" (selected-frame)))
```

#### 2. **Expand Treesitter Parsers**

Update `el-languages.el` to include all functional language parsers:

```elisp
(setq treesit-language-source-alist
      '(;; Systems
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")

        ;; Web
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")

        ;; Functional (ML family)
        (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src")
        (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "interface/src")
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
        (elm "https://github.com/elm-tooling/tree-sitter-elm")
        (gleam "https://github.com/gleam-lang/tree-sitter-gleam")

        ;; Functional (Lisp family)
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (racket "https://github.com/6cdh/tree-sitter-racket")
        (scheme "https://github.com/6cdh/tree-sitter-scheme")
        (commonlisp "https://github.com/theHamsta/tree-sitter-commonlisp")

        ;; Functional (BEAM)
        (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")

        ;; Functional (Other)
        (scala "https://github.com/tree-sitter/tree-sitter-scala")
        (nix "https://github.com/nix-community/tree-sitter-nix")))
```

#### 3. **Expand LSP Coverage**

Add missing LSP servers in `el-lsp.el`:

```elisp
;; Functional languages
(add-to-list 'eglot-server-programs '(gleam-mode . ("gleam" "lsp")))
(add-to-list 'eglot-server-programs '(purescript-mode . ("purescript-language-server" "--stdio")))
(add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))
(add-to-list 'eglot-server-programs '(erlang-mode . ("erlang_ls")))
(add-to-list 'eglot-server-programs '(scala-mode . ("metals")))
(add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
(add-to-list 'eglot-server-programs '(racket-mode . ("racket-langserver")))
```

#### 4. **Add Core Settings Parity**

In `el-core.el`, add:

```elisp
;; Scrolloff equivalent
(setq scroll-margin 8)
(setq scroll-conservatively 101)

;; Color column
(setq-default fill-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; System clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; Persistent undo
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
```

#### 5. **Install Missing Language Modes**

Add to `el-languages.el`:

```elisp
(use-package gleam-mode
  :ensure t
  :mode "\\.gleam\\'")

(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'")

(use-package clojure-mode
  :ensure t
  :mode "\\.clj[sc]?\\'")

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'")

(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'")

(use-package erlang
  :ensure t
  :mode ("\\.erl\\'" "\\.hrl\\'"))
```

---

### Medium Priority

#### 6. **Add Which-Key for Discoverability**

Create new file `el-which-key.el`:

```elisp
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(provide 'el-which-key)
```

#### 7. **Improve Git Integration**

In `el-dev-tools.el`, add:

```elisp
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
```

#### 8. **Modernize Completion UI**

Your Emacs already has good completion with `vertico`, but consider adding:

```elisp
;; In el-completion.el
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
```

---

### Low Priority (Nice to Have)

#### 9. **Session Management**

```elisp
(use-package desktop
  :ensure nil
  :config
  (desktop-save-mode 1)
  (setq desktop-path '("~/.emacs.d/desktop")
        desktop-dirname "~/.emacs.d/desktop"
        desktop-base-file-name "emacs.desktop"))
```

#### 10. **Color Scheme Migration**

Consider switching from `doom-one` to a transparent theme:

```elisp
;; Option 1: Use doom-one with transparency tweaks
(setq doom-one-brighter-comments t
      doom-one-comment-bg nil)

;; Option 2: Try Tokyo Night for Emacs
(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))
```

---

## 🚀 Implementation Strategy

1. **Phase 1: Core Parity** (Do this first)

   - Add core settings (scrolloff, color column, clipboard)
   - Add transparency support
   - Install `which-key`

2. **Phase 2: Language Support** (Essential for functional programming)

   - Expand treesitter parsers
   - Add missing LSP configurations
   - Install missing language modes

3. **Phase 3: Polish** (Optional enhancements)
   - Improve git integration
   - Enhance completion UI
   - Add session management

---

## 📝 Testing Checklist

After making changes:

- [ ] Transparency works in both GUI and terminal
- [ ] All LSP servers connect properly
- [ ] Treesitter parsers installed successfully
- [ ] Keybindings don't conflict
- [ ] Startup time remains reasonable
- [ ] All functional languages have syntax highlighting
- [ ] Git gutter shows changes properly

---

## 🔧 Quick Start

To begin modernization:

1. Backup your current config:

   ```bash
   cp -r ~/.emacs.d ~/.emacs.d.backup
   ```

2. Start with Phase 1 changes
3. Test thoroughly before moving to Phase 2
4. Iterate and adjust based on your preferences

---

**Note**: The goal is parity, not identical configs. Emacs and Neovim have different strengths. Keep what works well in Emacs (Org mode, built-in features) while adopting Neovim's modern improvements.
