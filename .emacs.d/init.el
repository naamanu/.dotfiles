;;; init.el --- Main Emacs configuration file -*- lexical-binding: t; -*-

;; Add the local lisp directory to the load path
(add-to-list 'load-path (expand-file-name "elisp/core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/plugins" user-emacs-directory))

;; Load Core modules
(require 'el-packages)
(require 'el-core)
(require 'el-bindings)

;; Load Plugin modules
(require 'el-theme)
(require 'el-which-key)
(require 'el-completion)
(require 'el-dev-tools)
(require 'el-lsp)
(require 'el-languages)
(require 'el-org)
(require 'el-treemacs)
(require 'el-vterm)
(require 'el-evil)
(require 'el-session)

(provide 'init)
;;; init.el ends here