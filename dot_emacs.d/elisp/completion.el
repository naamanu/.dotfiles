;;; completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Vertico + Orderless + Marginalia + Consult for the minibuffer; Corfu + Cape
;; in the buffer.  Embark supplies context actions over both.

;;; Code:

(defun my/setup-prog-completion ()
  "Add supplemental completion sources to programming buffers."
  (setq-local completion-cycle-threshold 3
              tab-always-indent 'complete)
  (add-hook 'completion-at-point-functions #'my/tempel-capf -90 t)
  (add-hook 'completion-at-point-functions #'cape-file nil t)
  (add-hook 'completion-at-point-functions #'cape-dabbrev t t))

(defun my/setup-text-completion ()
  "Add supplemental completion sources to text buffers."
  (setq-local completion-cycle-threshold 3
              tab-always-indent 'complete)
  (add-hook 'completion-at-point-functions #'my/tempel-capf -90 t)
  (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
  (add-hook 'completion-at-point-functions #'cape-file t t))

;; --- Minibuffer ----------------------------------------------------------

(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t))

;; Path-aware editing: DEL removes a whole directory component, RET on a
;; directory descends into it instead of confirming it, and shadowed prefixes
;; (`~/foo//etc') are tidied away.
(use-package vertico-directory
  :ensure nil ; ships inside vertico
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Space-separated components in any order.  Each component matches
;; literally, as a regexp, or as an initialism (`fbz' finds foo_bar.zig by
;; word initials), which keeps short queries precise; `foo~' forces true
;; subsequence (flex) matching when you want it, `=foo' literal only, `!foo'
;; excludes.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism))
  ;; Emacs's own per-category defaults (e.g. `basic' for buffers and emails)
  ;; would otherwise win over `completion-styles' in those categories.
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion orderless)))))

;; M-A in the minibuffer cycles between the annotators (e.g. more or less
;; detail for files and commands).
(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package consult
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref
        ;; `<' then a letter narrows consult-buffer and friends to one source.
        consult-narrow-key "<")
  :config
  ;; Searches that spawn a process preview on a short pause, not every
  ;; keystroke, so rg is not restarted for each character typed.
  (consult-customize consult-ripgrep consult-grep consult-git-grep
                     consult-fd consult-find
                     :preview-key '(:debounce 0.4 any))
  :bind
  (("C-x b"   . consult-buffer)
   ("C-x C-r" . consult-recent-file)
   ("C-x r b" . consult-bookmark)
   ("M-y"     . consult-yank-pop)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ("M-g o"   . consult-outline)
   ("M-g m"   . consult-mark)
   ("M-g M"   . consult-global-mark)
   ("M-g f"   . consult-flymake)
   ("M-s l"   . consult-line)
   ("M-s L"   . consult-line-multi)
   ("M-s r"   . consult-ripgrep)
   ("M-s f"   . consult-fd)
   ("M-s k"   . consult-keep-lines)
   ("M-s u"   . consult-focus-lines)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)))

;; Tree-sitter-aware region expansion: repeat to grow from symbol to
;; expression to statement to defun.
(use-package expreg
  :bind (("C-="   . expreg-expand)
         ("C-M-=" . expreg-contract)))

;; Replace the symbol at point across the buffer, defun, or above/below
;; point, without a query loop.  Bound under `C-c s u' in keys.el.
(use-package substitute
  :custom (substitute-highlight t))

;; `C-h' after any prefix (C-c n C-h, C-x C-h) lists its keys in the
;; minibuffer with completion instead of a help buffer.
(use-package embark
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-."   . embark-act)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; --- In-buffer -----------------------------------------------------------

;; Editor-style popup: the best candidate is preselected, TAB or RET
;; accepts it, C-n/C-p (C-j/C-k in evil) move, and the popup closes as soon
;; as nothing matches so RET is never hijacked on a plain word.
(use-package corfu
  :init (global-corfu-mode)
  :bind (:map corfu-map
         ("TAB"     . corfu-insert)
         ([tab]     . corfu-insert)
         ("S-TAB"   . corfu-previous)
         ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.08)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)          ; never auto-insert behind your back
  (corfu-preview-current nil)         ; no ghost text in the buffer while browsing
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-popupinfo-delay '(0.4 . 0.1))
  (corfu-min-width 30)
  (corfu-max-width 90)
  (corfu-count 12)
  :config
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1))

;; Snippets.  Type `<' and a name (`<match', `<def', `<src'): the matching
;; templates show in the completion popup; TAB accepts and expands (the `<'
;; goes away), then TAB / S-TAB move between fields.  `M-*' picks a template
;; from a list and `M-+' completes a bare name at point, no prefix needed.
;; Templates live in ~/.emacs.d/templates (chezmoi-managed); tempel-collection
;; adds a stock set for many modes.
(defun my/tempel-capf ()
  "`tempel-complete' as a capf that only fires after a `<' trigger.
Without the trigger, template names would shadow LSP candidates for any
word that happens to prefix-match one (tempel dropped its own
`tempel-trigger-prefix' option).  Falls through to the next capf when
nothing matches, so `a<b' comparisons are unaffected."
  (when (looking-back "<\\([[:alnum:]_*-]*\\)" (line-beginning-position))
    (let ((beg (match-beginning 1))
          (templates (tempel--templates)))
      (when templates
        (list beg (point) templates
              :category 'tempel
              :exclusive 'no
              :company-kind (lambda (_) 'snippet)
              :exit-function
              (lambda (name status)
                ;; Drop the trigger that sits just before the completed name.
                (save-excursion
                  (goto-char (- (point) (length name)))
                  (when (eq (char-before) ?<) (delete-char -1)))
                (tempel--exit templates nil name status)))))))

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("TAB"     . tempel-next)
         ([tab]     . tempel-next)
         ("S-TAB"   . tempel-previous)
         ([backtab] . tempel-previous)
         ("C-g"     . tempel-abort))
  :custom
  (tempel-path (expand-file-name "templates" user-emacs-directory)))

(use-package tempel-collection
  :after tempel)

(use-package cape
  :init
  (add-hook 'prog-mode-hook #'my/setup-prog-completion)
  (add-hook 'text-mode-hook #'my/setup-text-completion))

;; Icons in minibuffer candidates and the Corfu popup, when a Nerd Font is
;; installed.  That is a question for the display, so the setup runs from
;; `my/setup-appearance-hook' (core.el) once a GUI frame exists -- under a
;; daemon, not at load time.
(use-package nerd-icons-completion
  :defer t)

(use-package nerd-icons-corfu
  :defer t)

(defun my/setup-completion-icons ()
  "Show nerd-icons in marginalia annotations and the Corfu popup."
  (when (my/nerd-font-installed-p)
    (require 'nerd-icons-completion)
    (nerd-icons-completion-mode 1)
    (nerd-icons-completion-marginalia-setup)
    (require 'nerd-icons-corfu)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(add-hook 'my/setup-appearance-hook #'my/setup-completion-icons)

(provide 'completion)
;;; completion.el ends here
