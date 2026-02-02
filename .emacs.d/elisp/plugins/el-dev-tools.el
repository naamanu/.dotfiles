;;; --- 6. DEVELOPMENT TOOLS ---

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package forge
  :ensure t
  :after magit)

;; Diff-hl - inline change indicators (like Neovim's gitsigns)
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

(use-package projectile
  :init (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package consult
  :bind (("M-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)))

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

(provide 'el-dev-tools)
