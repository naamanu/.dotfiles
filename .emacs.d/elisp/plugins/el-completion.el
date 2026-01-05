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

;; In-buffer completion (Corfu) - matching Neovim's nvim-cmp
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)        ; Match Neovim's quick response
  (corfu-auto-prefix 2)         ; Trigger after 2 characters
  (corfu-cycle t)               ; Cycle through completions
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)   ; Don't preview current selection
  (corfu-on-exact-match nil))   ; Don't auto-insert exact match

(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'el-completion)
