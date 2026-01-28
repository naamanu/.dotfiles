;;; --- 4. THEME & APPEARANCE ---
(use-package nerd-icons
  :if (display-graphic-p))

;; Modus themes - high-quality, accessible light/dark themes (built-in to Emacs 28+)
;; Configure modus theme settings
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      ;; Syntax highlighting customization
      modus-themes-org-blocks 'gray-background
      modus-themes-headings '((1 . (1.3))
                              (2 . (1.2))
                              (t . (1.1))))

;; Load the light theme directly
(load-theme 'modus-operandi t)

;; Keep doom-themes available for fallback or switching
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; Optionally load a light doom theme instead:
  ;; (load-theme 'doom-one-light t)

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
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-project-detection 'auto))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(set-face-attribute 'default nil :font "Inconsolata" :height 165)

(provide 'el-theme)
