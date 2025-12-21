;;; --- 1. STARTUP OPTIMIZATION ---
(setq gc-cons-threshold (* 50 1024 1024)) ; 50MB during startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)))) ; 2MB for normal use

;; Set custom file to a separate location to keep init.el clean
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; --- 2. UI & DEFAULTS ---

(setq-default
 inhibit-startup-screen t
 initial-scratch-message ";; Welcome back!\n"
 use-dialog-box nil
 use-file-dialog nil
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows nil
 truncate-lines t
 indent-tabs-mode nil
 require-final-newline t
 sentence-end-double-space nil
 line-move-visual t
 visible-bell t
 show-trailing-whitespace t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(pixel-scroll-precision-mode 1)
(display-time-mode 1) ; Show time in the modeline

;; Remember state
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq history-length 25)

;;; --- 3. PACKAGE SYSTEM ---
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; --- 4. THEME & APPEARANCE ---
(use-package nerd-icons
  :if (display-graphic-p))

(setq modus-themes-region '(accented)
      modus-themes-mode-line '(:eval (list 'borderless 'accented)) ; Stylize the modeline
      modus-themes-completions '((matches . (extrabold underline)))
      modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-prompts '(bold)
      modus-themes-paren-match '(bold intense underline)
      modus-themes-syntax '(alt-syntax ansi-color-faint)
      modus-themes-syntax '(green-strings yellow-comments)
      )

(load-theme 'modus-operandi t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(set-face-attribute 'default nil :font "Fira Mono" :height 150)

;; Custom modeline styling
(set-face-attribute 'mode-line nil :background "#B18AE2" :foreground "#FFFFFF" :box '(:line-width 1 :color "#B18AE2"))
(set-face-attribute 'mode-line-inactive nil :background "#E0BBE4" :foreground "#000000" :box '(:line-width 1 :color "#E0BBE4"))

;;; --- 5. COMPLETION STACK (Vertico/Corfu) ---

;; Minibuffer completion (Vertico)
(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init (nerd-icons-completion-mode))

;; In-buffer completion (Corfu)
(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-quit-no-match 'separator))

(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)
            (hl-line-mode 1)
            (hs-minor-mode 1)))

;;; --- 6. DEVELOPMENT TOOLS ---

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package forge
  :ensure t
  :after magit)

(use-package projectile
  :init (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package consult
  :bind (("M-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)))

(use-package which-key
  :init (which-key-mode))

(use-package smartparens
  :init (smartparens-global-mode 1))

(use-package yasnippet
  :init (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package apheleia
  :init (apheleia-global-mode +1)) ; Background auto-formatting

(use-package wgrep)

(use-package avy
  :bind (("C-c j" . avy-goto-char-2)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package dap-mode
  :ensure t
  :config
  (dap-ui-mode 1)
  :bind (:map dap-mode-map
              ("C-c d b" . dap-add-breakpoint)
              ("C-c d d" . dap-debug)
              ("C-c d n" . dap-next)
              ("C-c d c" . dap-continue)))

;; Web, GraphQL, and SQL modes

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.jsx\\\'" . web-mode)))
(use-package astro-mode
  :ensure nil
  :mode ("\\.astro\\'" . astro-mode)
  :config
  (define-derived-mode astro-mode web-mode "astro"))

(use-package graphql-mode
  :ensure t
  :mode ("\\.graphql\\\'" . graphql-mode)
  :hook (graphql-mode . eglot-ensure))

(use-package sql
  :ensure nil ; Built-in
  :mode ("\\.sql\\\'" . sql-mode)
  :hook (sql-mode . eglot-ensure))

;; Elixir, Ruby, and Rails modes
(use-package elixir-mode
  :ensure t
  :mode "\\.exs?\\'")

(use-package ruby-mode
  :ensure nil ; Built-in
  :mode "\\.rb\\'")

(use-package rust-ts-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package projectile-rails
  :ensure t
  :after projectile
  :config (projectile-rails-global-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-language-source-alist
        '((rust "https://github.com/tree-sitter/tree-sitter-rust")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")))
  (global-treesit-auto-mode))


;;; --- 7. LSP (EGLOT) ---
;; NOTE: You need to install the language servers for eglot to work.
;; For example, for rust, you would run: rustup component add rust-analyzer
(use-package eglot
  :ensure nil ; Built-in
  :hook (prog-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode . ("elixir-ls")))
  (add-to-list 'eglot-server-programs '(ruby-mode . ("solargraph" "stdio")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(js-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '((c-ts-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs '(tuareg-mode . ("ocaml-lsp-server")))
  (add-to-list 'eglot-server-programs '(tuareg-ts-mode . ("ocaml-lsp-server")))
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls")))
  (add-to-list 'eglot-server-programs '(graphql-mode . ("graphql-language-server" "--stdio")))

  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.1
        eglot-hover-eldoc-documentation t
        eldoc-echo-area-use-multiline-p t)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format-buffer)
              ("C-c l r" . eglot-rename)
              ("C-c l d" . eglot-find-declaration)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l t" . eglot-find-type-definition)
              ("C-c l s" . consult-eglot-symbols)))

;;; --- 8. OCAML CONFIGURATION ---
(use-package tuareg
  :mode ("\\.ml[ily]?\\'" . tuareg-mode))

;; Ensure Opam environment is loaded into Emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
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

;;; --- 9. TERMINAL & KEYS ---
(use-package vterm
  :commands vterm)

(defun my/toggle-vterm ()
  "Toggle vterm terminal at bottom."
  (interactive)
  (if (get-buffer "*vterm*")
      (if (string= (buffer-name) "*vterm*")
          (delete-window)
        (pop-to-buffer "*vterm*"))
    (vterm)))

(global-set-key (kbd "C-`") 'my/toggle-vterm)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c w") 'delete-window)
(global-set-key (kbd "C-c r") 'replace-string)

;; Embark (Actions menu)
(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;;; --- 10. ORG MODE ---
(use-package org
  :ensure nil
  :config
  (setq org-agenda-files '("~/org"))
  (use-package org-superstar
    :ensure t
    :hook (org-mode . org-superstar-mode))
  :bind (("C-c a" . org-agenda)))

;;; --- 11. FILE EXPLORER ---
(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-git-mode 'simple)
  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("C-x t t" . treemacs)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-all-the-icons
  :after treemacs
  :ensure t
  :config (treemacs-load-theme "all-the-icons"))


