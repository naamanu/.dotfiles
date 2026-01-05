;; Web, GraphQL, and SQL modes
(use-package web-mode
  :ensure t
  :mode (("\.html?\'" . web-mode)
         ("\.jsx\\'" . web-mode)))
(use-package astro-mode
  :ensure nil
  :mode ("\.astro\\'" . astro-mode)
  :config
  (define-derived-mode astro-mode web-mode "astro"))

(use-package graphql-mode
  :ensure t
  :mode ("\.graphql\\'" . graphql-mode)
  :hook (graphql-mode . eglot-ensure))

(use-package sql
  :ensure nil ; Built-in
  :mode ("\.sql\\'" . sql-mode)
  :hook (sql-mode . eglot-ensure))

;; Elixir, Ruby, and Rails modes
(use-package elixir-mode
  :ensure t
  :mode "\.exs?\'")

(use-package ruby-mode
  :ensure nil ; Built-in
  :mode "\.rb\'")

(use-package rust-ts-mode
  :ensure t
  :mode ("\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure))

(use-package go-mode
  :ensure t
  :mode "\.go\'")

;; Functional languages - ML family

;; Gleam - simple mode with treesitter support (no MELPA package available)
(define-derived-mode gleam-mode prog-mode "Gleam"
  "Major mode for Gleam programming language."
  (setq-local comment-start "//")
  (setq-local comment-end ""))
(add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-mode))
(add-hook 'gleam-mode-hook #'eglot-ensure)

;; PureScript - simple mode with treesitter support
(define-derived-mode purescript-mode prog-mode "PureScript"
  "Major mode for PureScript programming language."
  (setq-local comment-start "--")
  (setq-local comment-end ""))
(add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))
(add-hook 'purescript-mode-hook #'eglot-ensure)

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :hook (elm-mode . eglot-ensure))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :hook (haskell-mode . eglot-ensure))

;; Functional languages - Lisp family
(use-package clojure-mode
  :ensure t
  :mode "\\.clj[sc]?\\'"
  :hook (clojure-mode . eglot-ensure))

;; Racket - simple mode (racket-mode not in standard MELPA)
(define-derived-mode racket-mode scheme-mode "Racket"
  "Major mode for Racket programming language."
  (setq-local comment-start ";")
  (setq-local comment-end ""))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(add-hook 'racket-mode-hook #'eglot-ensure)

(use-package scheme-mode
  :ensure nil ; Built-in
  :mode "\\.scm\\'")

;; Functional languages - BEAM ecosystem
(use-package erlang
  :ensure t
  :mode ("\\.erl\\'" "\\.hrl\\'")
  :hook (erlang-mode . eglot-ensure))

;; Functional languages - Other

;; Scala - simple mode with treesitter (scala-mode may not be available)
(define-derived-mode scala-mode prog-mode "Scala"
  "Major mode for Scala programming language."
  (setq-local comment-start "//")
  (setq-local comment-end ""))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-hook 'scala-mode-hook #'eglot-ensure)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure))

(use-package projectile-rails
  :ensure t
  :after projectile
  :config (projectile-rails-global-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-language-source-alist
        '(;; Systems languages
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          
          ;; Web languages
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          
          ;; Data formats
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          
          ;; Scripting languages
          (lua "https://github.com/tree-sitter/tree-sitter-lua")
          (vim "https://github.com/tree-sitter/tree-sitter-vim")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          
          ;; Functional languages - ML family
          (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src")
          (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "interface/src")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (elm "https://github.com/elm-tooling/tree-sitter-elm")
          (gleam "https://github.com/gleam-lang/tree-sitter-gleam")
          (purescript "https://github.com/postsolar/tree-sitter-purescript")
          
          ;; Functional languages - Lisp family
          (clojure "https://github.com/sogaiu/tree-sitter-clojure")
          (racket "https://github.com/6cdh/tree-sitter-racket")
          (scheme "https://github.com/6cdh/tree-sitter-scheme")
          (commonlisp "https://github.com/theHamsta/tree-sitter-commonlisp")
          
          ;; Functional languages - BEAM ecosystem
          (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          
          ;; Functional languages - Other
          (scala "https://github.com/tree-sitter/tree-sitter-scala")
          (nix "https://github.com/nix-community/tree-sitter-nix")))
  (global-treesit-auto-mode))

;;; --- 8. OCAML CONFIGURATION ---
(use-package tuareg
  :mode ("\.ml[ily]?\\'" . tuareg-mode))

;; Ensure Opam environment is loaded into Emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Use your existing opam-user-setup if needed
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" "Merlin mode" t)
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    (add-hook 'caml-mode-hook 'merlin-mode)))

;; Load OPAM setup if it exists
(let ((opam-setup (expand-file-name "~/.emacs.d/opam-user-setup.el")))
  (when (file-exists-p opam-setup)
    (load opam-setup)))

(provide 'el-languages)
