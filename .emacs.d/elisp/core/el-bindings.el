;;; --- 9. TERMINAL & KEYS ---
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

;; Enable CUA mode for familiar copy/paste/undo bindings (Cmd on Mac, Ctrl on Linux/Windows)
;; This uses Command key on macOS (s-c, s-v, s-x, s-z, s-a) which won't conflict
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy t)   ; Keep region active after copy

;; Additional undo/redo bindings
(global-set-key (kbd "C-z") 'undo-tree-undo)     ; Undo (Ctrl+Z)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)   ; Redo (Ctrl+Shift+Z)

;; Embark (Actions menu)
(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

(provide 'el-bindings)
