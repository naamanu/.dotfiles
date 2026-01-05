;;; --- 4. THEME & APPEARANCE ---
(use-package nerd-icons
  :if (display-graphic-p))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable transparency (like Neovim)
  ;; This sets the background to transparent
  (when (display-graphic-p)
    (set-frame-parameter nil 'alpha-background 90)
    (add-to-list 'default-frame-alist '(alpha-background . 90)))
  
  ;; For terminal Emacs - make background truly transparent
  (unless (display-graphic-p)
    (set-face-background 'default "unspecified-bg" (selected-frame))
    (set-face-background 'fringe "unspecified-bg" (selected-frame)))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom keywords
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 5)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-project-root-dir (replace-regexp-in-string (getenv "HOME") "~" (doom-project-root)))
  )

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(set-face-attribute 'default nil :font "Inconsolata" :height 165)

(provide 'el-theme)
