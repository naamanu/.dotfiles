;;; el-session.el --- Session and workspace management -*- lexical-binding: t; -*-

;; Desktop save mode for persistent sessions (like Neovim's auto-session)
;; Currently disabled - uncomment to enable session persistence
;; (use-package desktop
;;   :ensure nil ; Built-in
;;   :config
;;   (desktop-save-mode 1)
;;   (setq desktop-path (list (expand-file-name "desktop" user-emacs-directory))
;;         desktop-dirname (expand-file-name "desktop" user-emacs-directory)
;;         desktop-base-file-name "emacs.desktop"
;;         desktop-restore-eager t        ; Restore all buffers immediately (no lazy loading)
;;         desktop-auto-save-timeout 300) ; Auto-save every 5 minutes
;;
;;   ;; Create desktop directory if it doesn't exist
;;   (let ((desktop-dir (expand-file-name "desktop" user-emacs-directory)))
;;     (unless (file-directory-p desktop-dir)
;;       (make-directory desktop-dir t))))

(provide 'el-session)
;;; el-session.el ends here
