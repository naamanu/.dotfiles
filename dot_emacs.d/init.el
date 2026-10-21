;;; init.el --- Mostly-stock Emacs: evil, FP tooling, notes -*- lexical-binding: t; -*-

;;; Commentary:
;; Deliberately close to `emacs -Q': the default theme, mode line and keys,
;; plus only what Emacs lacks for this work --
;;   - evil (Vim keys) and vertico/consult for finding things,
;;   - Haskell and OCaml tooling (eglot with hls / ocamllsp, the same servers
;;     Neovim uses; GHCi, utop, dune), plus simple Python (basedpyright, ruff)
;;     and C/C++ (clangd) setups,
;;   - notes: Denote over ~/notes (shared with Neovim) and Org agenda/capture.
;; The previous, larger modular config is in the dotfiles git history.

;;; Code:

;; `emacs --batch -l init.el' (the validation command) implies -q, which
;; skips early-init.el and package activation.  Catch up so the check loads
;; what a real session does.
(unless (bound-and-true-p package-quickstart)
  (load (expand-file-name "early-init" user-emacs-directory) nil 'nomessage)
  (package-activate-all))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(require 'use-package)
(setq use-package-always-ensure t)

;; --- Basics ----------------------------------------------------------------

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 64 1024 1024)
                           gc-cons-percentage 0.1)))
;; Language servers send large JSON messages.
(setq read-process-output-max (* 1024 1024))

;; Backups, auto-saves and other state under var/, not next to every file.
(defconst my/var-dir (expand-file-name "var/" user-emacs-directory))
(make-directory (expand-file-name "auto-save/" my/var-dir) t)
(setq backup-directory-alist `(("." . ,(expand-file-name "backup/" my/var-dir)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" my/var-dir) t))
      create-lockfiles nil
      indent-tabs-mode nil)

;; Stock font (Menlo on macOS), 16pt -- the same size as Ghostty.  Applies
;; to every frame, including those emacsclient opens on a daemon.
(set-face-attribute 'default nil :height 160)

;; Remember minibuffer history, recent files and the place in each file.
(setq savehist-file (expand-file-name "history" my/var-dir)
      recentf-save-file (expand-file-name "recentf" my/var-dir)
      save-place-file (expand-file-name "places" my/var-dir))
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)

;; GUI Emacs on macOS does not inherit the shell PATH; without this ghc,
;; hls, opam, ocamllsp and utop are invisible.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (add-to-list 'exec-path-from-shell-variables "OPAM_SWITCH_PREFIX")
  (exec-path-from-shell-initialize))

;; theme-mode(1) pushes the shared light/dark mode to every editor.  This
;; Emacs keeps the stock theme, so there is nothing to switch.
(defun my/apply-theme-mode (_mode)
  "No-op: the stock theme is used regardless of the shared mode."
  nil)

;; --- Vim keys --------------------------------------------------------------

;; Pinned to MELPA: the NonGNU ELPA release predates Emacs 31 and errors with
;; (void-variable evil-mode-buffers) on every command.
(use-package evil
  :pin melpa
  :init
  ;; evil-collection needs these before evil loads.
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-redo
        evil-search-module 'isearch)
  :config
  (evil-set-leader '(normal visual motion) (kbd "SPC"))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer nil) ; keep vertico's keys
  :config (evil-collection-init))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1))

(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; --- Finding things --------------------------------------------------------

(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :init
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref)
  ;; TAB / C-M-i completion (LSP candidates included) through the minibuffer.
  (setq completion-in-region-function #'consult-completion-in-region)
  :bind (("C-x b"   . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-g f"   . consult-flymake)
         ("M-s l"   . consult-line)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-fd)))

;; Built-in (Emacs 30+): the top candidate as inline ghost text; TAB accepts.
(global-completion-preview-mode 1)

;; --- Python and C/C++ ------------------------------------------------------

;; Python: basedpyright (the server Neovim uses) through Eglot, ruff for
;; formatting, the built-in REPL (C-c C-p starts it, C-c C-c sends the
;; buffer, C-c C-r the region).
(defun my/ruff-format-buffer ()
  "Format the buffer with `ruff format', keeping it untouched on errors."
  (when (executable-find "ruff")
    (let ((out (generate-new-buffer " *ruff*"))
          (file (or buffer-file-name "stdin.py")))
      (unwind-protect
          (when (zerop (call-process-region (point-min) (point-max) "ruff" nil out nil
                                            "format" "--stdin-filename" file "-"))
            (replace-buffer-contents out))
        (kill-buffer out)))))

(defun my/python-mode-defaults ()
  "Defaults for Python buffers."
  (setq-local compile-command
              (if (locate-dominating-file default-directory "pyproject.toml")
                  "uv run pytest"
                "python3 -m pytest"))
  (add-hook 'before-save-hook #'my/ruff-format-buffer nil t)
  (my/eglot-ensure-when-executable "basedpyright-langserver"))
(add-hook 'python-mode-hook #'my/python-mode-defaults)
(add-hook 'python-ts-mode-hook #'my/python-mode-defaults)

;; C/C++: clangd through Eglot (it reads compile_commands.json; generate one
;; with `bear -- make' or CMake's -DCMAKE_EXPORT_COMPILE_COMMANDS=ON).
;; M-x compile runs `make' by default.
(defun my/c-mode-defaults ()
  "Defaults for C and C++ buffers."
  (setq-local c-basic-offset 4)
  (my/eglot-ensure-when-executable "clangd"))
(dolist (hook '(c-mode-hook c++-mode-hook c-ts-mode-hook c++-ts-mode-hook))
  (add-hook hook #'my/c-mode-defaults))

;; --- Haskell and OCaml -----------------------------------------------------

(defun my/eglot-ensure-when-executable (command)
  "Start Eglot only when COMMAND is on PATH, so buffers stay usable without it."
  (when (executable-find command)
    (eglot-ensure)))

;; Format through the server on save where the server can format: ormolu
;; via hls, ocamlformat via ocamllsp (with an .ocamlformat), clang-format via
;; clangd.  basedpyright cannot, so Python uses ruff below instead.
(defun my/eglot-format-on-save ()
  "Format through Eglot before saving, when the server supports it."
  (when (eglot-server-capable :documentFormattingProvider)
    (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

(use-package eglot
  :ensure nil
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format short))
  :hook (eglot-managed-mode . my/eglot-format-on-save)
  :config
  ;; Pin basedpyright: the built-in list would also accept ruff's server.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))))

(defun my/haskell-mode-defaults ()
  "Defaults for Haskell buffers."
  (setq-local compile-command "cabal build all")
  (my/eglot-ensure-when-executable "haskell-language-server-wrapper"))

;; interactive-haskell-mode: C-c C-l loads into GHCi, C-c C-z switches to it.
(use-package haskell-mode
  :hook ((haskell-mode . my/haskell-mode-defaults)
         (haskell-mode . interactive-haskell-mode)))

;; utop.el and dune.el ship with the opam packages of the same name, so the
;; elisp always matches the installed binaries.
(defconst my/opam-site-lisp
  (when-let* ((share (and (executable-find "opam")
                          (car (ignore-errors (process-lines "opam" "var" "share"))))))
    (let ((dir (expand-file-name "emacs/site-lisp" share)))
      (and (file-directory-p dir) dir)))
  "Emacs site-lisp directory of the active opam switch, or nil.")
(when my/opam-site-lisp
  (add-to-list 'load-path my/opam-site-lisp))

(defun my/ocaml-mode-defaults ()
  "Defaults for OCaml buffers."
  (setq-local compile-command "dune build")
  ;; Project libraries in utop inside a dune project, bare utop elsewhere.
  (setq-local utop-command
              (if (locate-dominating-file default-directory "dune-project")
                  "opam exec -- dune utop . -- -emacs"
                "opam exec -- utop -- -emacs"))
  (my/eglot-ensure-when-executable "ocamllsp"))

(use-package tuareg
  :mode (("\\.ml\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-mode))
  :hook (tuareg-mode . my/ocaml-mode-defaults))

;; Merlin features plain Eglot lacks: construct (fill a typed hole), destruct
;; (exhaustive match arms), typed-hole and enclosing-type navigation, type
;; search.  Keys on its own C-c C-* map.
(use-package ocaml-eglot
  :hook (tuareg-mode . ocaml-eglot-mode))

;; C-c C-e evaluates the phrase at point in utop, C-c C-b the buffer.
(use-package utop
  :ensure nil
  :if (and my/opam-site-lisp (locate-library "utop"))
  :hook (tuareg-mode . utop-minor-mode))

(use-package dune
  :ensure nil
  :if (and my/opam-site-lisp (locate-library "dune"))
  :mode ("\\(?:\\`\\|/\\)dune\\(?:-project\\|-workspace\\)?\\'" . dune-mode))

;; Home-grown, from ~/workspace/elisp: inline evaluation results for OCaml
;; and Haskell REPLs, and a Magit-style dune menu (C-c b).
(use-package fp-repl
  :ensure nil
  :if (file-directory-p "~/workspace/elisp/fp-repl")
  :load-path "~/workspace/elisp/fp-repl"
  :hook ((tuareg-mode haskell-mode) . fp-repl-mode))

;; Home-grown: mli-lens (the signature ocamllsp infers for this .ml, diffed
;; against the .mli you wrote) and osk-spec (odoc spec comments for a val).
(use-package ocaml-spec-kit
  :ensure nil
  :if (file-directory-p "~/workspace/elisp/ocaml-spec-kit")
  :load-path "~/workspace/elisp/ocaml-spec-kit"
  :hook (tuareg-mode . mli-lens-mode)
  :commands (mli-lens-show mli-lens-diff mli-lens-insert osk-spec-insert)
  :init
  (with-eval-after-load 'tuareg
    (define-key tuareg-mode-map (kbd "C-c i i") #'mli-lens-show)
    (define-key tuareg-mode-map (kbd "C-c i d") #'mli-lens-diff)
    (define-key tuareg-mode-map (kbd "C-c i w") #'mli-lens-insert)
    (define-key tuareg-mode-map (kbd "C-c i q") #'mli-lens-quit)
    (define-key tuareg-mode-map (kbd "C-c i s") #'osk-spec-insert)))

(use-package dune-transient
  :ensure nil
  :if (file-directory-p "~/workspace/elisp/dune-transient")
  :load-path "~/workspace/elisp/dune-transient"
  :commands dune-transient
  :init
  (with-eval-after-load 'tuareg
    (define-key tuareg-mode-map (kbd "C-c b") #'dune-transient)))

;; --- Notes -----------------------------------------------------------------

(defconst my/notes-directory
  (file-name-as-directory (expand-file-name (or (getenv "NOTES_DIR") "~/notes")))
  "Denote notes, shared with Neovim (lua/naamanu/core/notes.lua).")

(defconst my/org-directory (file-name-as-directory (expand-file-name "~/org"))
  "Org agenda and capture files.")

(defun my/org-file (name)
  "Return the path of NAME inside `my/org-directory'."
  (expand-file-name name my/org-directory))

(use-package org
  :ensure nil
  :custom
  (org-directory my/org-directory)
  (org-default-notes-file (my/org-file "inbox.org"))
  (org-agenda-files (list (my/org-file "inbox.org")
                          (my/org-file "projects.org")
                          (my/org-file "journal.org")))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "CANCELLED(c@)")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-capture-templates
   `(("t" "Todo" entry (file+headline ,(my/org-file "inbox.org") "Tasks")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a")
     ("i" "Idea" entry (file+headline ,(my/org-file "inbox.org") "Ideas")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a")
     ("p" "Project task" entry (file+headline ,(my/org-file "projects.org") "Active")
      "* NEXT %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n\n** Notes\n%?")))
  :hook (org-mode . visual-line-mode))

;; The notes are Markdown files.
(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

;; Markdown notes with YAML front matter: the exact format Neovim writes.
(use-package denote
  :custom
  (denote-directory my/notes-directory)
  (denote-file-type 'markdown-yaml)
  (denote-known-keywords
   '("ocaml" "haskell" "racket" "sml" "python" "ml" "c" "systems" "research" "reading"))
  (denote-prompts '(title keywords))
  :config (make-directory denote-directory t))

;; [title](denote:ID) links in Markdown.
(use-package denote-markdown
  :after denote)

;; One entry per day under journal/, tag `journal' (C-c n j).
(use-package denote-journal
  :after denote)

(use-package consult-denote
  :after denote
  :config (consult-denote-mode 1))

(defvar my/notes-map (make-sparse-keymap) "Notes, capture and agenda.")
(global-set-key (kbd "C-c n") my/notes-map)
(define-key my/notes-map (kbd "n") #'denote)
(define-key my/notes-map (kbd "j") #'denote-journal-new-or-existing-entry)
(define-key my/notes-map (kbd "f") #'consult-denote-find)
(define-key my/notes-map (kbd "s") #'consult-denote-grep)
(define-key my/notes-map (kbd "l") #'denote-link)
(define-key my/notes-map (kbd "b") #'denote-backlinks)
(define-key my/notes-map (kbd "r") #'denote-rename-file-using-front-matter)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; --- Leader (SPC in normal/visual state) -----------------------------------

(with-eval-after-load 'evil
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader> SPC") #'execute-extended-command
    (kbd "<leader> .")   #'find-file
    (kbd "<leader> ,")   #'consult-recent-file
    (kbd "<leader> b")   #'consult-buffer
    (kbd "<leader> /")   #'consult-ripgrep
    (kbd "<leader> p")   project-prefix-map
    (kbd "<leader> n")   my/notes-map
    (kbd "<leader> a")   #'org-agenda
    (kbd "<leader> c")   #'org-capture))

(provide 'init)
;;; init.el ends here
