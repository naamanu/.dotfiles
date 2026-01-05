;;; el-which-key.el --- Which-key for keybinding discovery -*- lexical-binding: t; -*-

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3
        which-key-prefix-prefix "+"
        which-key-sort-order 'which-key-key-order-alpha
        which-key-min-display-lines 3
        which-key-max-display-columns nil))

(provide 'el-which-key)
;;; el-which-key.el ends here
