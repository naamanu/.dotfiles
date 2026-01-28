;;; --- 1. STARTUP OPTIMIZATION ---
(setq gc-cons-threshold (* 50 1024 1024)) ; 50MB during startup

(add-hook 'emacs-startup-hook
          (lambda () 
            (setq gc-cons-threshold (* 16 1024 1024)))) ; 2MB for normal use

;; Set custom file to a separate location to keep init.el clean
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load shell environment variables (important for GUI Emacs)
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(add-hook 'prog-mode-hook
          (lambda () 
            (display-line-numbers-mode 1)
            (hl-line-mode 1)
            (hs-minor-mode 1)))

;;; --- 3. MODERN DEFAULTS (Neovim parity) ---

;; Scrolloff equivalent - keep cursor centered
(setq scroll-margin 8)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)

;; Color column at 100 (like Neovim)
(setq-default fill-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; System clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq save-interprogram-paste-before-kill t)

;; Persistent undo history
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo"))))

;; Create undo directory if it doesn't exist
(let ((undo-dir (expand-file-name "~/.emacs.d/undo")))
  (unless (file-directory-p undo-dir)
    (make-directory undo-dir t)))

(provide 'el-core)
