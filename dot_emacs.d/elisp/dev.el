;;; dev.el --- Projects, LSP, formatting, Git, terminal, debugging -*- lexical-binding: t; -*-

;;; Code:

;; --- Projects ------------------------------------------------------------

;; project.el (Emacs 31) covers the basics itself: `project-remember-project'
;; is a command, `project-root-find-file' is find-file from the root (new
;; files included), `project-dired' opens the root, `project-compile' builds
;; there.  Only the pieces with no built-in twin live here.

(defun my/project-root ()
  "Return the current project root, prompting for one outside a project."
  (expand-file-name (project-root (project-current t))))

(defun my/project-switch-find-file ()
  "Switch to a known project and go straight to `project-find-file'.
`project-switch-project' normally shows its command menu first; binding
`project-switch-commands' to one command skips it while keeping the
built-in prompt, history and project list."
  (interactive)
  (let ((project-switch-commands #'project-find-file))
    (call-interactively #'project-switch-project)))

(defun my/project-search ()
  "Ripgrep the current project."
  (interactive)
  (consult-ripgrep (my/project-root)))

(defun my/project-vterm ()
  "Pop the project's terminal, creating it on first use.
Outside a project the terminal belongs to `default-directory'.  `vterm'
given a name always makes a new buffer, so reuse is checked here."
  (interactive)
  (let* ((root (if-let* ((project (project-current nil)))
                   (expand-file-name (project-root project))
                 default-directory))
         (default-directory root)
         (name (format "*vterm: %s*"
                       (file-name-nondirectory (directory-file-name root)))))
    (if (get-buffer name)
        (pop-to-buffer name)
      (vterm name))))

(defun my/vterm-window ()
  "Return a window showing a vterm buffer on this frame, or nil."
  (seq-find (lambda (w)
              (with-current-buffer (window-buffer w)
                (derived-mode-p 'vterm-mode)))
            (window-list)))

(defun my/vterm-toggle ()
  "Show the project terminal, or hide it if one is visible.
Works from inside the terminal too: `C-c' is passed through to Emacs."
  (interactive)
  (if-let* ((win (my/vterm-window)))
      (if (one-window-p)
          (with-selected-window win (bury-buffer))
        (delete-window win))
    (my/project-vterm)))

(defvar my/project-test-command-alist
  '((typescript-ts-mode . my/node-test-command)
    (tsx-ts-mode        . my/node-test-command)
    (js-ts-mode         . my/node-test-command)
    (python-ts-mode     . my/python-test-command)
    (rust-ts-mode       . "cargo test")
    (go-ts-mode         . "go test ./...")
    (haskell-mode       . "cabal test all")
    (racket-mode        . "raco test .")
    (tuareg-mode        . "dune test"))
  "Default test command per major mode.
Values are either a literal string or a function returning one.")

(defun my/project-default-test-command ()
  "Return an appropriate test command for the current buffer."
  (let ((entry (alist-get major-mode my/project-test-command-alist)))
    (cond
     ((stringp entry) entry)
     ((and (symbolp entry) (fboundp entry)) (funcall entry))
     (t compile-command))))

(defun my/project-test ()
  "Run a mode-appropriate test command from the project root."
  (interactive)
  (let ((default-directory (my/project-root))
        (compile-command (my/project-default-test-command)))
    (call-interactively #'compile)))

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers
   '(".envrc" "package.json" "pyproject.toml" "Cargo.toml" "dune-project" "justfile"))
  :config
  (setq project-list-file (expand-file-name "projects" my/cache-dir)
        compilation-scroll-output 'first-error
        ;; `compile' and `project-compile' save modified buffers silently
        ;; instead of asking about each one, as an IDE would.
        compilation-ask-about-save nil))

;; Applies each project's direnv environment buffer-locally, so PATH,
;; VIRTUAL_ENV and friends resolve per project — LSP servers, ruff and
;; python all come from the project's own environment.  Enabled from
;; after-init as the envrc README advises, so envrc-mode is active before
;; other buffer-local setup runs.
(use-package envrc
  :if (executable-find "direnv")
  :hook (after-init . envrc-global-mode))

;; --- Shared terminal agent -----------------------------------------------

(defun my/agent-command ()
  "Return the configured terminal agent executable."
  (or (let ((configured (getenv "DEV_AGENT")))
        (and configured (not (string-empty-p configured)) configured))
      (seq-find #'executable-find '("codex" "claude"))))

(defun my/project-agent ()
  "Open or reuse the shared terminal coding agent at the project root."
  (interactive)
  (let ((agent (my/agent-command)))
    (unless agent
      (user-error "Install codex or claude, or set DEV_AGENT"))
    (let ((default-directory (my/project-root))
          (name (format "*agent: %s*" (file-name-nondirectory
                                       (directory-file-name (my/project-root))))))
      (if (get-buffer name)
          (pop-to-buffer name)
        (vterm name)
        (vterm-send-string agent)
        (vterm-send-return)))))

(defun my/agent-context (beg end)
  "Copy a file-and-line context reference for BEG through END, then open the agent."
  (interactive "r")
  (unless buffer-file-name (user-error "Save this buffer before sharing context"))
  (let* ((root (my/project-root))
         (path (file-relative-name buffer-file-name root))
         (first (line-number-at-pos beg))
         (last (line-number-at-pos end)))
    (kill-new (format "Please inspect @%s:%d-%d" path first last))
    (my/project-agent)
    (message "Context reference copied; paste it into the agent terminal")))

;; --- LSP -----------------------------------------------------------------

(defun my/eglot-ensure-when-executable (command)
  "Start Eglot only when COMMAND is on PATH, so buffers stay usable without it."
  (when (executable-find command)
    (eglot-ensure)))

(defun my/eglot-completion-styles ()
  "Let orderless match the server's candidates in Eglot-managed buffers."
  (setq-local completion-category-overrides
              '((eglot (styles orderless basic))
                (file (styles partial-completion)))))

;; Deferred: `eglot-ensure' is autoloaded, so Eglot loads with the first
;; buffer that wants it rather than at startup (it pulls in ert, jsonrpc,
;; xref, flymake...).
(use-package eglot
  :ensure nil
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  ;; Logging every JSON-RPC message is the single biggest Eglot slowdown on
  ;; busy servers.  Off by default; set `:size 2000000' temporarily and
  ;; restart the server when debugging a language server.
  (eglot-events-buffer-config '(:size 0 :format short))
  :config
  ;; NOTE: never add `flymake' to `eglot-stay-out-of'.  That list is matched as
  ;; a regexp against variable names, so "flymake" also matches
  ;; `flymake-diagnostic-functions' and prevents Eglot from installing its
  ;; diagnostics backend — silently disabling every LSP diagnostic.
  ;; Eglot enables eldoc, flymake and (where the server offers them) inlay
  ;; hints itself; do not hook them on again here.  `C-c l h' toggles hints.
  ;;
  ;; Emacs 31's own `eglot-server-programs' already lists every server this
  ;; config uses -- rust-analyzer, gopls, clangd, ocamllsp, basedpyright,
  ;; millet, the vscode json/css servers, ... -- together with the
  ;; `:language-id' each expects (tuareg-mode -> "ocaml", tsx-ts-mode ->
  ;; "typescriptreact").  Re-adding entries here shadows those and loses the
  ;; ids, so the only addition is vtsls, which the built-in list does not
  ;; know; pushed to the front, it wins over typescript-language-server.
  (add-to-list 'eglot-server-programs
               '(((js-ts-mode :language-id "javascript")
                  (typescript-ts-mode :language-id "typescript")
                  (tsx-ts-mode :language-id "typescriptreact"))
                 . ("vtsls" "--stdio")))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-completion-styles))

;; Workspace-wide symbol search through the language server (`C-c l s').
(use-package consult-eglot
  :after (consult eglot))

;; Documentation at point in a childframe, on demand (`K' in normal state);
;; the echo-area eldoc stays as it is.
(use-package eldoc-box
  :commands (eldoc-box-help-at-point))

(with-eval-after-load 'flymake
  (setq flymake-fringe-indicator-position 'right-fringe
        flymake-show-diagnostics-at-end-of-line 'short))

;; Eglot runs one server per buffer, so ruff's lint rules arrive through
;; Flymake directly rather than as a second LSP.  Registered from
;; `eglot-managed-mode-hook' because Eglot resets the diagnostic backends when
;; it takes over a buffer; langs.el handles the no-basedpyright fallback.
(use-package flymake-ruff
  :if (executable-find "ruff")
  :init
  (defun my/flymake-ruff-after-eglot ()
    "Add ruff's diagnostics once Eglot manages a Python buffer.
The hook also runs when Eglot shuts down, so only act while it is on."
    (when (and (derived-mode-p 'python-base-mode)
               (bound-and-true-p eglot--managed-mode))
      (flymake-ruff-load)))
  (add-hook 'eglot-managed-mode-hook #'my/flymake-ruff-after-eglot))

;; --- Formatting ----------------------------------------------------------

(defun my/apheleia-inhibit-in-notes-p ()
  "Non-nil in buffers under the Denote directory, so Apheleia stays off there.
Prettier would reflow every Markdown note on save; Denote manages their
front matter and links itself.  `my/notes-directory' (notes.el) is the
fallback for the second before denote itself has loaded."
  (when-let* ((dir (or (bound-and-true-p denote-directory)
                       (bound-and-true-p my/notes-directory))))
    (and buffer-file-name (file-in-directory-p buffer-file-name dir))))

;; Apheleia formats asynchronously on save without moving point.
(use-package apheleia
  :custom
  (apheleia-mode-alist
   '((typescript-ts-mode . prettier)
     (tsx-ts-mode        . prettier)
     (js-ts-mode         . prettier)
     (json-ts-mode       . prettier)
     (yaml-ts-mode       . prettier)
     (css-ts-mode        . prettier)
     (markdown-mode      . prettier)
     (python-ts-mode     . (ruff-isort ruff-format)) ; sort imports, then format
     (rust-ts-mode       . rustfmt)
     (c-ts-mode          . clang-format)
     (c++-ts-mode        . clang-format)
     (cmake-ts-mode      . cmake-format)
     (go-ts-mode         . goimports)
     (haskell-mode       . ormolu)
     (lua-ts-mode        . stylua)
     (bash-ts-mode       . shfmt)
     (tuareg-mode        . ocamlformat)
     (sql-mode           . sql-formatter)))
  :config
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  ;; Without a project .clang-format, leave the buffer alone rather than
  ;; imposing LLVM style; formatting stays project-opt-in like ocamlformat.
  ;; The whole command is spelled out, rather than spliced into apheleia's
  ;; own list, so reloading this file stays idempotent.
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--fallback-style=none" "-assume-filename"
          (or (apheleia-formatters-local-buffer-file-name)
              (apheleia-formatters-mode-extension)
              ".c")))
  (add-to-list 'apheleia-inhibit-functions #'my/apheleia-inhibit-in-notes-p)
  (apheleia-global-mode 1))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; --- Git -----------------------------------------------------------------

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-save-repository-buffers 'dontask))

;; Forge puts GitHub pull requests and issues in the Magit status buffer.
;; It reuses the token from `gh auth`, read via auth-source.  Its default
;; Magit bindings are off because evil-collection (vim.el) installs its own
;; Vim-compatible set and warns otherwise.
(use-package forge
  :after magit
  :custom (forge-add-default-bindings nil))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (diff-hl-flydiff-mode 1))

;; Editable grep buffers: export from consult with `C-.' then E, edit, C-c C-c.
;; Loads with the first grep buffer (its autoloads hook grep-mode).
(use-package wgrep
  :defer t)

;; --- Terminal ------------------------------------------------------------

(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000)
  :init
  ;; Open vterm in a side split on the right instead of replacing the
  ;; current window; reuse the window if a vterm is already showing.
  (add-to-list 'display-buffer-alist
               '("\\*vterm"
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.4))))

;; --- Debugging -----------------------------------------------------------

;; dape is the DAP client built for Eglot; it replaces dap-mode, which belonged
;; to the lsp-mode ecosystem.  Adapters ship as built-in configurations, so
;; only the binaries need installing.  lldb-dap is a global install (on macOS
;; setup.sh links the one from brew's llvm into ~/.local/bin); debugpy
;; is a Python module dape imports via the project's own `python' (envrc puts
;; the venv on PATH), so add it per project with `uv add --dev debugpy' —
;; dape errors clearly when it is missing.
(use-package dape
  :commands (dape dape-breakpoint-toggle)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t)
  :config
  (dape-breakpoint-global-mode 1))

(provide 'dev)
;;; dev.el ends here
