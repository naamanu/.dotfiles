;;; --- 1. STARTUP OPTIMIZATION ---
(setq gc-cons-threshold (* 50 1024 1024)) ; 50MB during startup

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024)))) ; 2MB for normal use

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

(use-package doom-themes
  :config
  (setq doom-themes-enable-italic t
        doom-themes-enable-bold t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-icon t))

(set-face-attribute 'default nil :font "Fira Mono" :height 120)

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

;;; --- 6. DEVELOPMENT TOOLS ---

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package consult
  :bind (("C-s" . consult-line)
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
  :bind ("M-s" . avy-goto-char-2))

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
(use-package eglot
  :ensure nil ; Built-in
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (tuareg-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.1)
  :bind (:map eglot-mode-map
              ("C-c C-r" . eglot-rename)
              ("C-c C-f" . eglot-format-buffer)
              ("C-c l f" . eglot-format)
              ("C-c l a" . eglot-code-actions)))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-c C-s" . consult-eglot-symbols)))

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
