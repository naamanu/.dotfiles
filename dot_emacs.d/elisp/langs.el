;;; langs.el --- Tree-sitter grammars and per-language setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Scope: Python (incl. notebooks), the FP stack (Racket, Standard ML, Haskell,
;; OCaml), systems programming (C, C++, Rust, CMake), TypeScript/JavaScript,
;; Go, Lua, shell, SQL, plus YAML, Dockerfile, JSON, TOML, CSV and Markdown.

;;; Code:

;; --- Tree-sitter ---------------------------------------------------------

;; Grammars support more highlighting than the default level 3 enables.
(setq treesit-font-lock-level 4)

;; Emacs 31 does the mode plumbing itself.  With `treesit-enabled-modes' set
;; to t every built-in tree-sitter mode takes over from its classic
;; counterpart (python-ts-mode for python-mode, c-ts-mode for c-mode, ...),
;; and the `*-ts-mode-maybe' wrappers it puts in `auto-mode-alist' fall back
;; to the classic mode, offering to build the grammar first, when one is
;; missing.  `setopt' matters: the option's setter is what installs the
;; remaps.  Racket, SML, Haskell and OCaml keep their classic major modes,
;; which are still more complete than the tree-sitter ones.
(setopt treesit-enabled-modes t
        treesit-auto-install-grammar 'ask)

;; Emacs ships tree-sitter support but no grammars; `C-c e g'
;; (`my/install-missing-grammars') compiles them into ~/.emacs.d/tree-sitter/.
;; Each entry is pinned to the commit the corresponding Emacs 31 mode
;; declares for itself -- the grammar version it was tested against -- so a
;; rebuild on a new machine produces the same grammar, not whatever HEAD is.
;; The doc-comment grammars (`jsdoc' for js-ts-mode, `doxygen' for
;; c-ts-mode) and go.mod / go.work are included because the modes declare
;; them and would otherwise offer to build them on first use.  Bump the
;; commits together with an Emacs upgrade.
(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash"
                    :commit "487734f87fd87118028a65a4599352fa99c9cde8")
        (c          "https://github.com/tree-sitter/tree-sitter-c"
                    :commit "3aa2995549d5d8b26928e8d3fa2770fd4327414e")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp"
                    :commit "f41b4f66a42100be405f96bdc4ebc4a61095d3e8")
        (cmake      "https://github.com/uyha/tree-sitter-cmake"
                    :commit "e409ae33f00e04cde30f2bcffb979caf1a33562a")
        (css        "https://github.com/tree-sitter/tree-sitter-css"
                    :commit "6a442a3cf461b0ce275339e5afa178693484c927")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"
                    :commit "087daa20438a6cc01fa5e6fe6906d77c869d19fe")
        (doxygen    "https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
                    :commit "1e28054cb5be80d5febac082706225e42eff14e6")
        (go         "https://github.com/tree-sitter/tree-sitter-go"
                    :commit "12fe553fdaaa7449f764bc876fd777704d4fb752")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod"
                    :commit "3b01edce2b9ea6766ca19328d1850e456fde3103")
        (gowork     "https://github.com/omertuc/tree-sitter-go-work"
                    :commit "949a8a470559543857a62102c84700d291fc984c")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                    :commit "108b2d4d17a04356a340aea809e4dd5b801eb40d")
        (jsdoc      "https://github.com/tree-sitter/tree-sitter-jsdoc"
                    :commit "b253abf68a73217b7a52c0ec254f4b6a7bb86665")
        (json       "https://github.com/tree-sitter/tree-sitter-json"
                    :commit "4d770d31f732d50d3ec373865822fbe659e47c75")
        (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua"
                    :commit "db16e76558122e834ee214c8dc755b4a3edc82a9")
        (python     "https://github.com/tree-sitter/tree-sitter-python"
                    :commit "bffb65a8cfe4e46290331dfef0dbf0ef3679de11")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust"
                    :commit "18b0515fca567f5a10aee9978c6d2640e878671a")
        (toml       "https://github.com/tree-sitter-grammars/tree-sitter-toml"
                    :commit "64b56832c2cffe41758f28e05c756a3a98d16f41")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                    :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
                    :source-dir "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    :commit "8e13e1db35b941fc57f2bd2dd4628180448c17d5"
                    :source-dir "typescript/src")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
                    :commit "b733d3f5f5005890f324333dd57e1f0badec5c87")))

(defun my/install-missing-grammars (&optional force)
  "Compile every tree-sitter grammar that is not already available.
With prefix argument FORCE, reinstall grammars that are already present."
  (interactive "P")
  (let (installed failed)
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (when (or force (not (treesit-language-available-p lang)))
        (condition-case err
            (progn (treesit-install-language-grammar lang)
                   (push lang installed))
          (error (push (cons lang (error-message-string err)) failed)))))
    (message "Grammars installed: %s%s"
             (if installed (mapconcat #'symbol-name (nreverse installed) ", ") "none")
             (if failed (format " | failed: %s" (mapcar #'car failed)) ""))))

;; --- Shared helpers ------------------------------------------------------

(defun my/set-local-compile-command (command)
  "Set the buffer-local `compile-command' to COMMAND."
  (setq-local compile-command command))

(defun my/project-file-path (name)
  "Return the absolute path to NAME at the project root, or nil."
  (when-let* ((project (project-current nil)))
    (let ((path (expand-file-name name (project-root project))))
      (when (file-exists-p path) path))))

;; --- JavaScript / TypeScript / React -------------------------------------

(defun my/node-project-root ()
  "Return the current Node project root, if there is one."
  (when-let* ((project (project-current nil)))
    (let ((root (project-root project)))
      (when (file-exists-p (expand-file-name "package.json" root)) root))))

(defun my/node-package-manager (root)
  "Return the package manager command for ROOT, detected from the lockfile."
  (cond
   ((file-exists-p (expand-file-name "bun.lockb" root)) "bun")
   ((file-exists-p (expand-file-name "bun.lock" root)) "bun")
   ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) "pnpm")
   ((file-exists-p (expand-file-name "yarn.lock" root)) "yarn")
   (t "npm")))

(defun my/node-run-script-command (script)
  "Return a command that runs package SCRIPT with the project's package manager."
  (when-let* ((root (my/node-project-root)))
    (let ((manager (my/node-package-manager root)))
      (if (string= manager "yarn")
          (format "yarn %s" script)
        (format "%s run %s" manager script)))))

(defun my/node-test-command ()
  "Return the test command for the current Node project."
  (or (my/node-run-script-command "test") "npm test"))

(defun my/js-mode-defaults ()
  "Defaults for JavaScript, TypeScript and TSX buffers."
  (setq-local tab-width 2 fill-column 100)
  (when-let* ((cmd (or (my/node-run-script-command "typecheck")
                       (my/node-run-script-command "build"))))
    (my/set-local-compile-command cmd))
  (my/eglot-ensure-when-executable "vtsls"))

(dolist (hook '(js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook #'my/js-mode-defaults))

;; --- Rust ----------------------------------------------------------------

(defun my/rust-mode-defaults ()
  "Defaults for Rust buffers."
  (my/set-local-compile-command "cargo check")
  (setq-local fill-column 100)
  (my/eglot-ensure-when-executable "rust-analyzer"))

(add-hook 'rust-ts-mode-hook #'my/rust-mode-defaults)

;; --- C / C++ -------------------------------------------------------------

;; clangd reads compile_commands.json for flags; generate one in Makefile
;; projects with `bear -- make'.  A project .clang-format or .editorconfig
;; overrides the indentation defaults below.
(defun my/c-mode-defaults ()
  "Defaults for C and C++ buffers."
  (my/set-local-compile-command
   (if (my/project-file-path "CMakeLists.txt") "cmake --build build" "make -k"))
  (setq-local tab-width 4 fill-column 100)
  (setq-local c-ts-indent-offset 4) ; `c-ts-mode-indent-offset' is obsolete in 31
  (my/eglot-ensure-when-executable "clangd"))

(dolist (hook '(c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook #'my/c-mode-defaults))

;; CMakeLists.txt and *.cmake open in the built-in `cmake-ts-mode';
;; cmake-language-server adds completion and diagnostics when installed, and
;; Apheleia formats with cmake-format (dev.el).
(defun my/cmake-mode-defaults ()
  "Defaults for CMake buffers."
  (my/set-local-compile-command "cmake --build build")
  (my/eglot-ensure-when-executable "cmake-language-server"))

(add-hook 'cmake-ts-mode-hook #'my/cmake-mode-defaults)

;; K&R brace placement with the 4-column offset set above.  `linux' lays
;; braces out the same way but is the kernel style, 8-column tabs included,
;; which contradicts the spaces-only indentation used everywhere else here.
(use-package c-ts-mode
  :ensure nil
  :custom (c-ts-mode-indent-style 'k&r))

;; --- Lisps: Racket and Emacs Lisp ----------------------------------------

;; Structural editing for s-expression languages only; elsewhere the plain
;; `electric-pair-mode' from core.el stays in charge.  REPL buffers are left
;; out so RET always submits input.
(defconst my/lisp-mode-hooks
  '(emacs-lisp-mode-hook lisp-interaction-mode-hook lisp-mode-hook
    scheme-mode-hook racket-mode-hook)
  "Hooks of source modes that get paredit.")

(use-package paredit
  :init
  (dolist (hook my/lisp-mode-hooks)
    (add-hook hook #'enable-paredit-mode))
  :config
  ;; paredit inserts and balances its own pairs.
  (add-hook 'paredit-mode-hook (lambda () (electric-pair-local-mode -1)))
  ;; Keep M-j for avy (core.el) and M-s for consult's search map.
  (define-key paredit-mode-map (kbd "M-j") nil)
  (define-key paredit-mode-map (kbd "M-s") nil))

;; racket-mode talks to a Racket back end of its own, which gives it
;; check-syntax-driven navigation, rename, eldoc and Flymake diagnostics
;; (`racket-xp-mode') without an LSP.  C-c C-c runs the file, C-c C-z visits
;; the REPL, C-M-x sends the definition at point.
(defun my/racket-mode-defaults ()
  "Defaults for Racket buffers."
  (my/set-local-compile-command "raco test .")
  (setq-local fill-column 100))

(use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :hook ((racket-mode . my/racket-mode-defaults)
         (racket-mode . racket-xp-mode))
  :custom
  (racket-program "racket")
  (racket-show-functions '(racket-show-echo-area)))

;; --- Standard ML ---------------------------------------------------------

;; sml-mode drives the SML/NJ REPL (`sml-run', C-c C-l loads the buffer,
;; C-c C-r the region).  millet-ls adds diagnostics and navigation; it
;; expects a `millet.toml' or a lone project root and is optional.
(defun my/sml-mode-defaults ()
  "Defaults for Standard ML buffers."
  (setq-local fill-column 100)
  (my/eglot-ensure-when-executable "millet-ls"))

(use-package sml-mode
  :mode (("\\.sml\\'" . sml-mode)
         ("\\.sig\\'" . sml-mode)
         ("\\.fun\\'" . sml-mode))
  :hook (sml-mode . my/sml-mode-defaults)
  :custom
  (sml-program-name "sml")
  (sml-indent-level 2))

;; --- Go, Haskell, Lua and shell ------------------------------------------

(defun my/go-mode-defaults ()
  (my/set-local-compile-command "go test ./...")
  (setq-local tab-width 4 fill-column 100)
  (my/eglot-ensure-when-executable "gopls"))
(add-hook 'go-ts-mode-hook #'my/go-mode-defaults)

(defun my/haskell-mode-defaults ()
  (my/set-local-compile-command "cabal build all")
  (setq-local fill-column 100)
  (my/eglot-ensure-when-executable "haskell-language-server-wrapper"))
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook ((haskell-mode . my/haskell-mode-defaults)
         ;; Owns C-c C-l (load into GHCi), C-c C-z (switch to REPL), etc.
         (haskell-mode . interactive-haskell-mode)))

;; `lua-ts-mode' is built in since Emacs 30; needs the lua grammar.
(defun my/lua-mode-defaults ()
  (setq-local tab-width 2 fill-column 100)
  (my/eglot-ensure-when-executable "lua-language-server"))
(add-hook 'lua-ts-mode-hook #'my/lua-mode-defaults)

(defun my/shell-mode-defaults ()
  (my/set-local-compile-command "shellcheck .")
  (my/eglot-ensure-when-executable "bash-language-server"))
(add-hook 'bash-ts-mode-hook #'my/shell-mode-defaults)

;; --- OCaml ---------------------------------------------------------------

(defun my/ocaml-mode-defaults ()
  "Defaults for OCaml buffers."
  (my/set-local-compile-command "dune build")
  ;; matches the janestreet ocamlformat profile's margin (~/.config/ocamlformat)
  (setq-local fill-column 90)
  ;; utop.el reads a buffer-local `utop-command' from the buffer that
  ;; launches it: project libraries via dune inside a project, bare utop
  ;; elsewhere.
  (setq-local utop-command
              (if (my/project-file-path "dune-project")
                  "opam exec -- dune utop . -- -emacs"
                "opam exec -- utop -- -emacs"))
  (my/eglot-ensure-when-executable "ocamllsp"))

(use-package tuareg
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode))
  :hook (tuareg-mode . my/ocaml-mode-defaults))

;; utop.el and dune.el ship with the opam packages of the same name, not
;; ELPA, so the elisp always matches the installed binaries.  Their directory
;; comes from the active switch (`opam var share') rather than a hardcoded
;; ~/.opam/default; one short opam call at startup.
(defconst my/opam-site-lisp
  (when-let* ((share (and (executable-find "opam")
                          (car (ignore-errors (process-lines "opam" "var" "share"))))))
    (let ((dir (expand-file-name "emacs/site-lisp" share)))
      (and (file-directory-p dir) dir)))
  "Emacs site-lisp directory of the active opam switch, or nil without one.")

(when my/opam-site-lisp
  (add-to-list 'load-path my/opam-site-lisp))

(defun my/opam-site-lisp-has-p (file)
  "Whether FILE exists in `my/opam-site-lisp'."
  (and my/opam-site-lisp (file-exists-p (expand-file-name file my/opam-site-lisp))))

(use-package utop
  :ensure nil
  :if (my/opam-site-lisp-has-p "utop.el")
  :commands (utop utop-minor-mode)
  :hook (tuareg-mode . utop-minor-mode)
  :custom
  (utop-edit-command nil))

;; Syntax for `dune', `dune-project' and `dune-workspace' files.
(use-package dune
  :ensure nil
  :if (my/opam-site-lisp-has-p "dune.el")
  :mode ("\\(?:\\`\\|/\\)dune\\(?:-project\\|-workspace\\)?\\'" . dune-mode))

;; Home-grown: eros-style inline evaluation results across the FP stack —
;; OCaml (utop), Haskell (GHCi), Racket and Standard ML behind one set of
;; keys.  Supersedes utop-eros, which did OCaml alone; enabling both would
;; advise the same utop functions twice.
(use-package fp-repl
  :ensure nil
  :if (file-directory-p "~/workspace/elisp/fp-repl")
  :load-path "~/workspace/elisp/fp-repl"
  :hook ((tuareg-mode haskell-mode racket-mode sml-mode) . fp-repl-mode))

;; Home-grown: Magit-style menu for dune (build/test/exec/fmt/promote
;; with composable switches).  Bound to C-c b in keys.el.
(use-package dune-transient
  :ensure nil
  :if (file-directory-p "~/workspace/elisp/dune-transient")
  :load-path "~/workspace/elisp/dune-transient"
  :commands (dune-transient))

;; Home-grown: the signature ocamllsp infers for the current .ml, and a diff
;; against the .mli you wrote.  Keys under C-c i in keys.el.
(use-package mli-lens
  :ensure nil
  :if (file-directory-p "~/workspace/elisp/mli-lens")
  :load-path "~/workspace/elisp/mli-lens"
  :hook (tuareg-mode . mli-lens-mode)
  :commands (mli-lens-show mli-lens-diff mli-lens-insert))

;; Merlin features plain Eglot drops: `ocaml-eglot-construct' fills a typed
;; hole, `ocaml-eglot-destruct' generates exhaustive match arms, plus
;; type-driven search and enclosing-type navigation.  Its C-c C-i / C-c C-l
;; jumps are unbound in keys.el (they shadow the tab map; xref covers them).
(use-package ocaml-eglot
  :hook (tuareg-mode . ocaml-eglot-mode))

;; --- Python --------------------------------------------------------------

;; No hand-rolled venv detection: envrc supplies the project environment where
;; an .envrc exists, uv runs commands inside the project venv without
;; activation, and basedpyright finds a root-level .venv on its own.
(defun my/python-uv-project-p ()
  "Whether the current project should be driven through uv."
  (and (executable-find "uv") (my/project-file-path "pyproject.toml")))

(defun my/python-test-command ()
  "Return a test command for the current Python project."
  (if (my/python-uv-project-p) "uv run pytest" "python3 -m pytest"))

(defun my/python-set-interpreter ()
  "Point `run-python' at the best interpreter for this buffer.
Prefers the project venv's ipython, then its python, then a global ipython
\(installed with `uv tool install ipython'), then python3.  envrc has
already put the venv on PATH, so `executable-find' sees it."
  (let ((ipython (executable-find "ipython")))
    (if ipython
        (setq-local python-shell-interpreter ipython
                    python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
      (setq-local python-shell-interpreter (or (executable-find "python3") "python3")
                  python-shell-interpreter-args "-i"))))

(defun my/python-mode-defaults ()
  "Defaults for Python buffers."
  (setq-local tab-width 4 fill-column 88)
  (my/set-local-compile-command
   (let ((file (shell-quote-argument (or buffer-file-name ""))))
     (if (my/python-uv-project-p)
         (format "uv run python %s" file)
       (format "python3 %s" file))))
  (my/python-set-interpreter)
  (if (executable-find "basedpyright-langserver")
      (eglot-ensure) ; flymake-ruff attaches from eglot-managed-mode-hook (dev.el)
    (when (fboundp 'flymake-ruff-load)
      (flymake-ruff-load)
      (flymake-mode 1))))

(add-hook 'python-ts-mode-hook #'my/python-mode-defaults)

;; --- Notebooks and data files ---------------------------------------------

;; Cell-based editing over plain .py files with `# %%' markers; cells are sent
;; to the inferior Python REPL with C-c C-c.  With jupytext on PATH, .ipynb
;; notebooks open transparently as scripts and convert back on save.
(use-package code-cells
  :init
  (when (executable-find "jupytext")
    (add-to-list 'auto-mode-alist '("\\.ipynb\\'" . code-cells-convert-ipynb)))
  :hook (python-ts-mode . code-cells-mode-maybe)
  :bind (:map code-cells-mode-map
              ("M-p"     . code-cells-backward-cell)
              ("M-n"     . code-cells-forward-cell)
              ("C-c C-c" . code-cells-eval)))

;; tsv-mode derives from csv-mode, so the alignment hook covers both.
(use-package csv-mode
  :mode (("\\.csv\\'" . csv-mode)
         ("\\.tsv\\'" . tsv-mode))
  :hook (csv-mode . csv-align-mode))

;; --- SQL, YAML, Docker ---------------------------------------------------

;; SQL has no LSP worth the weight; `sql-mode' is built in and Apheleia
;; handles formatting via sql-formatter.
(defun my/simple-web-defaults ()
  "Shared indentation defaults for config and markup buffers."
  (setq-local tab-width 2 fill-column 100))

(dolist (hook '(sql-mode-hook css-ts-mode-hook json-ts-mode-hook toml-ts-mode-hook))
  (add-hook hook #'my/simple-web-defaults))

(defun my/yaml-mode-defaults ()
  "Defaults for YAML buffers."
  (my/simple-web-defaults)
  (my/eglot-ensure-when-executable "yaml-language-server"))

(add-hook 'yaml-ts-mode-hook #'my/yaml-mode-defaults)

(defun my/dockerfile-mode-defaults ()
  "Defaults for Dockerfile buffers."
  (my/simple-web-defaults)
  (my/eglot-ensure-when-executable "docker-langserver"))

(add-hook 'dockerfile-ts-mode-hook #'my/dockerfile-mode-defaults)

;; --- Markdown and Emacs Lisp ---------------------------------------------

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :hook (markdown-mode . visual-line-mode))

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(provide 'langs)
;;; langs.el ends here
