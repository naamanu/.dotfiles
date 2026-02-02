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
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
