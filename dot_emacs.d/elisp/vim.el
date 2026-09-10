;;; vim.el --- Vim keys (evil) -*- lexical-binding: t; -*-

;;; Commentary:
;; Vim modal editing on top of the rest of the config, not instead of it:
;; every `C-c' prefix map still works, and keys.el mirrors them under a
;; `SPC' leader in normal and visual state.  Everything Emacs-specific
;; (Consult, Embark, Corfu, xref) is reached from the same keys as before.

;;; Code:

;; Pinned to MELPA: the 1.15.0 release on NonGNU ELPA predates Emacs 31,
;; whose `define-globalized-minor-mode' no longer defines `evil-mode-buffers',
;; so every post-command errors with (void-variable evil-mode-buffers).
(use-package evil
  :pin melpa
  :init
  ;; evil-collection requires these to be set before evil loads.
  (setq evil-want-integration t
        evil-want-keybinding nil
        ;; C-u scrolls as in Vim; the universal argument stays on C-M-u.
        evil-want-C-u-scroll t
        evil-want-C-u-delete t
        ;; Y behaves like D and C (to end of line), not like yy.
        evil-want-Y-yank-to-eol t
        ;; j/k move by visual line where visual-line-mode is on (prose).
        evil-respect-visual-line-mode t
        ;; * and # search the symbol at point, so `foo-bar' is one thing.
        evil-symbol-word-search t
        evil-split-window-below t
        evil-vsplit-window-right t
        ;; Built-in undo-redo (Emacs 28+): u undoes, C-r redoes, no tree
        ;; package needed.
        evil-undo-system 'undo-redo
        ;; Keep isearch: C-s/C-r and consult-line stay Emacs-native; / and ?
        ;; still search.
        evil-search-module 'isearch
        ;; Shift width follows the buffer's indentation.
        evil-shift-width 2)
  :config
  ;; REPL buffers start in insert state.  evil's own
  ;; `evil-insert-state-modes' already lists comint, utop, racket-repl,
  ;; haskell-interactive, inferior-sml and inferior-python, so there is
  ;; nothing to add; setting the variable here would *replace* that list.
  ;; evil-collection gives dape's buffers their Vim keys in normal state.
  ;; Leader lives in keys.el; declare it here so `evil-define-key' forms
  ;; using <leader> resolve.
  (evil-set-leader '(normal visual motion) (kbd "SPC"))
  (evil-mode 1))

;; Vim-style bindings for Magit, Dired, vterm, pdf-tools, Org agenda,
;; compilation, xref, Corfu, dape and the rest.
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil) ; keep Vertico's own keys
  (evil-collection-want-unimpaired-p t)  ; [b ]b [e ]e etc.
  :config
  (evil-collection-init))

;; ys / cs / ds and S in visual state.
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

;; gcc, gc<motion>, and gc in visual state.
(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1))

;; Editing an Org heading with vim motions: gj/gk/gh/gl move by headings,
;; M-h/j/k/l promote/demote/move, and TAB cycles in normal state.
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'vim)
;;; vim.el ends here
