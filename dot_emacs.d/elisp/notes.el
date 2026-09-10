;;; notes.el --- Org, Denote, writing and presentations -*- lexical-binding: t; -*-

;;; Commentary:
;; Org handles capture and agenda over a small fixed set of files.  Denote
;; handles durable, linkable notes: plain files whose names encode date, title
;; and keywords, with no database to rebuild.

;;; Code:

(defconst my/notes-directory
  (file-name-as-directory (expand-file-name (or (getenv "NOTES_DIR") "~/notes")))
  "Portable Markdown note directory.")

(defconst my/bibliography-file
  (expand-file-name (or (getenv "BIBLIOGRAPHY") "references.bib") my/notes-directory)
  "Shared BibTeX bibliography.")

(defconst my/org-directory (file-name-as-directory (expand-file-name "~/org"))
  "Org agenda and capture files.  Denote notes live in `my/notes-directory'.")

(defun my/org-file (name)
  "Return the path of NAME inside `my/org-directory'."
  (expand-file-name name my/org-directory))

(defun my/find-org-inbox ()
  "Open the Org inbox (C-c n i)."
  (interactive)
  (find-file (my/org-file "inbox.org")))

(defun my/find-org-projects ()
  "Open the Org projects file (C-c n p)."
  (interactive)
  (find-file (my/org-file "projects.org")))

;; --- Org -----------------------------------------------------------------

(use-package org
  :ensure nil
  :custom
  (org-directory my/org-directory)
  (org-default-notes-file (my/org-file "inbox.org"))
  ;; journal.org holds the entries captured before denote-journal took over
  ;; (C-c n j, below); it stays on the agenda for those.
  (org-agenda-files (list (my/org-file "inbox.org")
                          (my/org-file "projects.org")
                          (my/org-file "journal.org")))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "|" "DONE(d)" "CANCELLED(c@)")))
  (org-tag-alist '(("@research" . ?r) ("@coding" . ?c) ("@writing" . ?w) ("@admin" . ?a)))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-return-follows-link t)
  (org-catch-invisible-edits 'show)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-use-fast-todo-selection 'expert)
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-capture-templates
   `(("t" "Todo" entry (file+headline ,(my/org-file "inbox.org") "Tasks")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a")
     ("i" "Idea" entry (file+headline ,(my/org-file "inbox.org") "Ideas")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a")
     ("p" "Project task" entry (file+headline ,(my/org-file "projects.org") "Active")
      "* NEXT %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n\n** Notes\n%?")))
  ;; org-cite: the shared bibliography, with citar as the processor (its
  ;; autoloads register it the moment org-cite loads).
  (org-cite-global-bibliography (list my/bibliography-file))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 1)))
            (todo "NEXT")
            (todo "TODO" ((org-agenda-overriding-header "Inbox")))
            (tags-todo "@coding")
            (tags-todo "@research")))
          ("r" "Research" tags-todo "@research")
          ("w" "Writing" tags-todo "@writing")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (python . t) (shell . t) (haskell . t) (ocaml . t) (C . t)))
  ;; SVG previews stay crisp on retina displays; dvisvgm ships with MacTeX.
  (when (executable-find "dvisvgm")
    (setq org-preview-latex-default-process 'dvisvgm)))

;; --- Denote --------------------------------------------------------------

;; Loaded one idle second after startup rather than during it: denote
;; requires dired, which brings dirvish and transient along.  Commands are
;; autoloaded, so using one earlier just loads it then.
(use-package denote
  :defer 1
  :custom
  (denote-directory my/notes-directory)
  (denote-file-type 'markdown-yaml)
  (denote-known-keywords
   '("emacs" "typescript" "rust" "ocaml" "haskell" "racket" "sml" "python" "ml"
     "c" "sql" "systems" "research" "project" "reading"))
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  :config
  (make-directory denote-directory t)
  ;; Show the readable title in the buffer name rather than the raw filename.
  (denote-rename-buffer-mode 1))

;; Denote's prompts through consult, with previews: `consult-denote-find' and
;; `consult-denote-grep' (C-c n f / C-c n s) replace a bare find-file and
;; ripgrep in the notes directory, and the mode adds a notes source to
;; `consult-buffer'.
(use-package consult-denote
  :after denote
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  (consult-denote-find-command (if (executable-find "fd") #'consult-fd #'consult-find))
  :config (consult-denote-mode 1))

;; Link conversion for Markdown notes (Denote <-> Obsidian <-> plain file
;; paths), the format `denote-file-type' selects above.
(use-package denote-markdown
  :after denote)

;; Org extras: dynamic blocks of links and backlinks, links to headings.
(use-package denote-org
  :after (denote org))

;; Journal entries as Denote notes -- one file per day in the `journal'
;; subdirectory, keyword `journal' -- instead of an Org datetree.  `C-c n j'
;; opens today's entry, creating it if needed.
(use-package denote-journal
  :after denote)

;; --- Papers and bibliography ---------------------------------------------

;; Real PDF rendering.  The epdfinfo server compiles itself the first time a
;; PDF is opened; the build needs poppler, automake and pkg-config (setup.sh).
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init (pdf-loader-install :no-query)
  :custom (pdf-view-use-scaling t)) ; crisp text on retina displays

;; One bibliography for everything: org-cite inserts citations (C-c C-x @),
;; citar browses, previews and opens entries.  Its commands are autoloaded
;; and citar-org's autoloads register the org-cite processors when org-cite
;; loads, so nothing here waits for Org: `citar-open' (C-c n c) works in a
;; Markdown-only session too.
(use-package citar
  :defer t
  :custom
  (citar-bibliography (list my/bibliography-file)))

;; Literature notes as Denote files: `citar-open' gains actions to create
;; and revisit a note per bibliography entry.
(use-package citar-denote
  :after citar
  :config (citar-denote-mode 1))

(declare-function citar-select-ref "citar")

(defun my/citar-insert-pandoc ()
  "Insert a Pandoc citation using the shared bibliography."
  (interactive)
  (require 'citar)
  (when-let* ((key (citar-select-ref)))
    (insert (format "[@%s]" key))))

;; --- Writing -------------------------------------------------------------

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star 'replace)
  (org-modern-table nil)) ; leave table alignment to Org itself

(use-package olivetti
  :hook ((org-mode . olivetti-mode)
         (markdown-mode . olivetti-mode))
  :custom (olivetti-body-width 90))

;; Prose in the `variable-pitch' face (Charter, set in core.el); code blocks,
;; tables, and metadata stay `fixed-pitch' via `modus-themes-mixed-fonts'.
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'markdown-mode-hook #'variable-pitch-mode)

;; --- LaTeX and math ------------------------------------------------------

;; AUCTeX makes .tex files first class: `C-c C-c' runs latexmk, the PDF opens
;; in pdf-tools and SyncTeX links source and PDF both ways (`C-c C-v', or a
;; click in the PDF); RefTeX handles labels, references and citations; texlab
;; adds completion and diagnostics through Eglot when installed (the built-in
;; `eglot-server-programs' already maps LaTeX-mode to it).  AUCTeX also
;; supplies `texmathp' (is point inside math?), without which
;; `org-cdlatex-mode' errors in every Org buffer.
(declare-function TeX-source-correlate-mode "tex")
(declare-function TeX-revert-document-buffer "tex")

(defun my/latex-mode-defaults ()
  "Defaults for AUCTeX LaTeX buffers."
  (setq-local TeX-command-default "LaTeXMk")
  (TeX-source-correlate-mode 1)
  (reftex-mode 1)
  (my/eglot-ensure-when-executable "texlab"))

(use-package auctex
  :defer t
  :custom
  (TeX-parse-self t)                     ; parse on open, for completion and RefTeX
  (TeX-source-correlate-start-server t)  ; let the viewer jump back to the source
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (reftex-plug-into-AUCTeX t)
  :hook (LaTeX-mode . my/latex-mode-defaults))

;; `auctex' the feature is never loaded as such (tex-site autoloads `tex' and
;; `latex'), so hooks on its internals go through the real library.
(with-eval-after-load 'tex
  ;; Refresh the PDF buffer once a compile finishes.
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; Fast LaTeX math entry inside Org: ` opens a symbol menu, _ and ^ insert
;; scripts with braces, TAB expands templates like fr and env names.
(use-package cdlatex
  :hook (org-mode . org-cdlatex-mode))

;; Render a LaTeX fragment when the cursor leaves it, un-render on re-entry.
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; Just-in-time spellchecker; needs the enchant library.
(use-package jinx
  :hook ((text-mode . jinx-mode)
         (org-mode . jinx-mode))
  :bind ("M-$" . jinx-correct))

;; --- Presentations -------------------------------------------------------

;; Defined before the `use-package' form that hooks them, so the byte-compiler
;; sees the definitions first.
(declare-function org-present-big "org-present")
(declare-function org-present-small "org-present")
(declare-function org-present-hide-cursor "org-present")
(declare-function org-present-show-cursor "org-present")
(declare-function org-present-read-only "org-present")
(declare-function org-present-read-write "org-present")

(defun my/org-present-start ()
  "Enter a distraction-free presentation view."
  (org-present-big)
  ;; Org 9.8 renamed `org-display-inline-images' to this.
  (org-link-preview-region t t (point-min) (point-max))
  (org-present-hide-cursor)
  (org-present-read-only)
  (display-line-numbers-mode -1)
  (olivetti-mode 1))

(defun my/org-present-stop ()
  "Restore the buffer after presenting."
  (org-present-small)
  (org-link-preview-clear (point-min) (point-max))
  (org-present-show-cursor)
  (org-present-read-write))

(use-package org-present
  :commands org-present
  :hook ((org-present-mode . my/org-present-start)
         (org-present-mode-quit . my/org-present-stop)))
(provide 'notes)
;;; notes.el ends here
