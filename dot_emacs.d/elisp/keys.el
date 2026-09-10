;;; keys.el --- Global keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded last so every referenced command and map already exists.
;; Prefixes: C-c p project, C-c s search, C-c l LSP, C-c n notes,
;;           C-c d debug, C-c f REPL, C-c t toggles, C-c e Emacs,
;;           C-c TAB tabs (workspaces).
;; Evil: the same prefixes under SPC in normal/visual state (see Leader).
;; Package-owned bindings kept on their upstream defaults: consult's M-g /
;; M-s maps and marginalia's M-A (completion.el), expreg on C-= (completion.el),
;; dape, magit.
;; Known shadows: Org and Markdown bind C-c ' (edit block) and C-c TAB in
;; their own maps, so in those buffers the terminal toggle and the tab map
;; are reached from the leader (SPC ' / SPC TAB) or via M-x.

;;; Code:

;; --- Global --------------------------------------------------------------

;; ESC quits the thing in progress (region, minibuffer, recursive edit,
;; running command) and nothing else -- see `my/escape-dwim' in core.el.
(global-set-key (kbd "<escape>") #'my/escape-dwim)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-c w") #'delete-window)
;; Also works inside the terminal: vterm passes C-c through to Emacs.
(global-set-key (kbd "C-c '") #'my/vterm-toggle)

;; --- Toggles and Emacs -------------------------------------------------

(defvar my/toggle-map (make-sparse-keymap) "Appearance and editor toggles.")
(global-set-key (kbd "C-c t") my/toggle-map)
(define-key my/toggle-map (kbd "t") #'my/toggle-theme)
(define-key my/toggle-map (kbd "l") #'display-line-numbers-mode)
(define-key my/toggle-map (kbd "w") #'visual-line-mode)
(define-key my/toggle-map (kbd "o") #'olivetti-mode)
(define-key my/toggle-map (kbd "f") #'fontaine-set-preset)
(define-key my/toggle-map (kbd "s") #'my/sidebar-toggle)
(define-key my/toggle-map (kbd "S") #'my/sidebar-focus)
(define-key my/toggle-map (kbd "i") #'indent-bars-mode)
(define-key my/toggle-map (kbd "h") #'hl-line-mode)

;; --- Tabs (workspaces) ---------------------------------------------------

(defvar my/tab-map (make-sparse-keymap) "Tab-bar workspaces.")
(global-set-key (kbd "C-c TAB") my/tab-map)
(define-key my/tab-map (kbd "TAB") #'tab-bar-switch-to-tab)
(define-key my/tab-map (kbd "n") #'tab-bar-new-tab)
(define-key my/tab-map (kbd "x") #'tab-bar-close-tab)
(define-key my/tab-map (kbd "r") #'tab-bar-rename-tab)
(define-key my/tab-map (kbd "]") #'tab-bar-switch-to-next-tab)
(define-key my/tab-map (kbd "[") #'tab-bar-switch-to-prev-tab)
(define-key my/tab-map (kbd "l") #'tab-bar-switch-to-recent-tab)

(defvar my/emacs-map (make-sparse-keymap) "Commands about Emacs itself.")
(global-set-key (kbd "C-c e") my/emacs-map)
(define-key my/emacs-map (kbd "h") #'my/emacs-health-check)
(define-key my/emacs-map (kbd "g") #'my/install-missing-grammars)

;; --- Search --------------------------------------------------------------

(defvar my/search-map (make-sparse-keymap) "Search and navigation.")
(global-set-key (kbd "C-c s") my/search-map)
(define-key my/search-map (kbd "s") #'my/project-search)
(define-key my/search-map (kbd "r") #'consult-ripgrep)
(define-key my/search-map (kbd "l") #'consult-line)
(define-key my/search-map (kbd "o") #'consult-outline)
(define-key my/search-map (kbd "i") #'consult-imenu)
(define-key my/search-map (kbd "f") #'consult-fd)
(define-key my/search-map (kbd "d") #'consult-flymake)
;; C-c s u {b,d,r,s}: substitute in buffer / defun / below (rest) / above (start).
;; The map is an autoloaded keymap symbol: bound by name, it loads
;; substitute on first use.
(define-key my/search-map (kbd "u") 'substitute-prefix-map)

;; --- Projects ------------------------------------------------------------

;; A map of our own rather than `project-prefix-map', which stays pristine
;; for `C-x p' and for the menu `project-switch-project' shows (find-regexp,
;; shell, vc-dir, any-command...).
(defvar my/project-map (make-sparse-keymap) "Project commands.")
(global-set-key (kbd "C-c p") my/project-map)
(define-key my/project-map (kbd "p") #'project-switch-project)
(define-key my/project-map (kbd "P") #'my/project-switch-find-file)
(define-key my/project-map (kbd "f") #'project-find-file)
(define-key my/project-map (kbd "o") #'project-root-find-file) ; new files too
(define-key my/project-map (kbd "b") #'project-switch-to-buffer)
(define-key my/project-map (kbd "a") #'project-remember-project)
(define-key my/project-map (kbd "d") #'project-find-dir)
(define-key my/project-map (kbd "D") #'project-dired)
(define-key my/project-map (kbd "s") #'my/project-search)
(define-key my/project-map (kbd "m") #'project-compile)
(define-key my/project-map (kbd "t") #'my/project-test)
(define-key my/project-map (kbd "v") #'my/project-vterm)
(define-key my/project-map (kbd "g") #'my/project-agent)
(define-key my/project-map (kbd "k") #'project-kill-buffers)

;; --- LSP -----------------------------------------------------------------

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l d") #'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c l D") #'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c l i") #'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l t") #'eglot-find-typeDefinition)
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l s") #'consult-eglot-symbols)
  (define-key eglot-mode-map (kbd "C-c l e") #'flymake-show-buffer-diagnostics)
  (define-key eglot-mode-map (kbd "C-c l n") #'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "C-c l p") #'flymake-goto-prev-error)
  (define-key eglot-mode-map (kbd "C-c l h") #'eglot-inlay-hints-mode))
;; Diagnostics are also on M-g f (consult-flymake) and `[e ]e'; M-g n / M-g p
;; keep their stock meaning, `next-error' / `previous-error'.

;; --- Debugging -----------------------------------------------------------

(defvar my/debug-map (make-sparse-keymap) "Debugger commands (dape).")
(global-set-key (kbd "C-c d") my/debug-map)
(define-key my/debug-map (kbd "d") #'dape)
(define-key my/debug-map (kbd "b") #'dape-breakpoint-toggle)
(define-key my/debug-map (kbd "n") #'dape-next)
(define-key my/debug-map (kbd "i") #'dape-step-in)
(define-key my/debug-map (kbd "o") #'dape-step-out)
(define-key my/debug-map (kbd "c") #'dape-continue)
(define-key my/debug-map (kbd "q") #'dape-quit)

;; --- Notes ---------------------------------------------------------------

(defvar my/notes-map (make-sparse-keymap) "Notes, capture and agenda.")
(global-set-key (kbd "C-c n") my/notes-map)
(define-key my/notes-map (kbd "n") #'denote)
(define-key my/notes-map (kbd "l") #'denote-link)
(define-key my/notes-map (kbd "b") #'denote-backlinks)
(define-key my/notes-map (kbd "r") #'denote-rename-file)
(define-key my/notes-map (kbd "f") #'consult-denote-find)
(define-key my/notes-map (kbd "s") #'consult-denote-grep)
(define-key my/notes-map (kbd "j") #'denote-journal-new-or-existing-entry)
(define-key my/notes-map (kbd "c") #'citar-open)
(define-key my/notes-map (kbd "C") #'my/citar-insert-pandoc)
(define-key my/notes-map (kbd "i") #'my/find-org-inbox)
(define-key my/notes-map (kbd "p") #'my/find-org-projects)

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)

;; --- OCaml / dune --------------------------------------------------------

;; The dune menu, in OCaml source and dune files.
(with-eval-after-load 'tuareg
  (define-key tuareg-mode-map (kbd "C-c b") #'dune-transient)
  ;; C-c i: the inferred-interface lens.
  (define-key tuareg-mode-map (kbd "C-c i i") #'mli-lens-show)
  (define-key tuareg-mode-map (kbd "C-c i d") #'mli-lens-diff)
  (define-key tuareg-mode-map (kbd "C-c i w") #'mli-lens-insert)
  (define-key tuareg-mode-map (kbd "C-c i q") #'mli-lens-quit))
(with-eval-after-load 'dune
  (define-key dune-mode-map (kbd "C-c b") #'dune-transient))

;; ocaml-eglot binds C-c C-i (= C-c TAB, the tab map) and C-c C-l to
;; declaration/definition jumps that xref already covers (gd, C-c l d).
(with-eval-after-load 'ocaml-eglot
  (keymap-unset ocaml-eglot-mode-map "C-c C-i" 'remove)
  (keymap-unset ocaml-eglot-mode-map "C-c C-l" 'remove))

;; --- FP inline evaluation (fp-repl) ---------------------------------------

;; `interactive-haskell-mode-map' binds C-c C-v to haskell-cabal-visit-file
;; and, being a minor mode too, wins over `fp-repl-mode-map'.  Claim it back
;; so the key means the same thing in every FP buffer.
(with-eval-after-load 'haskell
  (when (boundp 'interactive-haskell-mode-map)
    (define-key interactive-haskell-mode-map (kbd "C-c C-v") #'fp-repl-eval-phrase)))

;; --- Terminal agent -----------------------------------------------------

(defvar my/agent-map (make-sparse-keymap) "Shared terminal-agent commands.")
(global-set-key (kbd "C-c g") my/agent-map)
(define-key my/agent-map (kbd "g") #'my/project-agent)
(define-key my/agent-map (kbd "c") #'my/agent-context)

;; --- REPLs ---------------------------------------------------------------

;; Only REPLs with real Emacs support; for node, a project vterm (C-c p v)
;; beats a bare comint buffer.  utop qualifies: utop.el speaks a dedicated
;; -emacs protocol with completion and phrase evaluation.  Haskell uses the
;; GHCi session of `interactive-haskell-mode' (langs.el), the one C-c C-l
;; loads into -- not the older inf-haskell `run-haskell', a second REPL that
;; knew nothing about it.  In-buffer bindings (C-c C-c / C-c C-l / C-c C-z)
;; come from each mode; these only open REPLs.
(defvar my/repl-map (make-sparse-keymap) "Language REPLs.")
(global-set-key (kbd "C-c f") my/repl-map)
(define-key my/repl-map (kbd "p") #'run-python)
(define-key my/repl-map (kbd "s") #'sql-product-interactive)
(define-key my/repl-map (kbd "e") #'ielm)
(define-key my/repl-map (kbd "o") #'utop)
(define-key my/repl-map (kbd "r") #'racket-repl)
(define-key my/repl-map (kbd "m") #'sml-run)
(define-key my/repl-map (kbd "h") #'haskell-interactive-switch)

;; --- Leader (evil) -------------------------------------------------------

;; `SPC' in normal/visual state mirrors the `C-c' prefixes below, so `SPC p f'
;; is `C-c p f'.  Extra leader keys that have no C-c twin: SPC SPC (M-x),
;; SPC b (buffers), SPC v (Magit), SPC w (windows), SPC ; (comment).
(with-eval-after-load 'evil
  ;; Same ESC as everywhere else (core.el): quit, never rearrange windows.
  (define-key evil-normal-state-map (kbd "<escape>") #'my/escape-dwim)
  (evil-define-key '(normal visual motion) 'global
    (kbd "<leader> SPC") #'execute-extended-command
    (kbd "<leader> b")   #'consult-buffer
    (kbd "<leader> B")   #'project-switch-to-buffer
    (kbd "<leader> .")   #'find-file
    (kbd "<leader> ,")   #'consult-recent-file
    (kbd "<leader> v")   #'magit-status
    (kbd "<leader> w")   evil-window-map
    (kbd "<leader> x")   #'embark-act
    (kbd "<leader> c")   #'org-capture
    (kbd "<leader> a")   #'org-agenda
    (kbd "<leader> j")   #'avy-goto-char-timer
    (kbd "<leader> p")   my/project-map
    (kbd "<leader> s")   my/search-map
    (kbd "<leader> n")   my/notes-map
    (kbd "<leader> d")   my/debug-map
    (kbd "<leader> f")   my/repl-map
    (kbd "<leader> t")   my/toggle-map
    (kbd "<leader> e")   my/emacs-map
    (kbd "<leader> g")   my/agent-map
    (kbd "<leader> TAB") my/tab-map
    (kbd "<leader> '")   #'my/vterm-toggle
    (kbd "<leader> o")   #'my/sidebar-toggle
    (kbd "<leader> O")   #'my/sidebar-focus)
  ;; gt / gT switch tabs, as in Vim.
  (evil-define-key '(normal motion) 'global
    (kbd "gt") #'tab-bar-switch-to-next-tab
    (kbd "gT") #'tab-bar-switch-to-prev-tab)
  ;; Vim habits: gd / gr / K on top of xref and eldoc, everywhere.
  (evil-define-key 'normal 'global
    (kbd "gd") #'xref-find-definitions
    (kbd "gr") #'xref-find-references
    (kbd "K")  #'eldoc-box-help-at-point)
  ;; Region expansion in visual state: v grows, - shrinks (V keeps its
  ;; linewise meaning).
  (evil-define-key 'visual 'global
    (kbd "v") #'expreg-expand
    (kbd "-") #'expreg-contract))

;; SPC l is the LSP map only in Eglot-managed buffers, like C-c l.
(with-eval-after-load 'eglot
  (with-eval-after-load 'evil
    (evil-define-key '(normal visual) eglot-mode-map
      (kbd "<leader> l") (lookup-key eglot-mode-map (kbd "C-c l"))
      (kbd "gi")         #'eglot-find-implementation
      (kbd "gy")         #'eglot-find-typeDefinition)))

(provide 'keys)
;;; keys.el ends here
