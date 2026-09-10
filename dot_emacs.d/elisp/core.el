;;; core.el --- Editor defaults, session state, appearance -*- lexical-binding: t; -*-

;;; Code:

;; Restore a working GC threshold now that startup is done.  64MB keeps GC
;; pauses rare during LSP and completion bursts without hoarding memory.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))

;; --- Responsiveness ------------------------------------------------------

;; Language servers send large JSON messages; the 4kB default makes Emacs
;; read them in hundreds of tiny chunks.
(setq read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil)

;; Do not fontify while keys are being typed; scroll without computing
;; exact line heights first; keep font caches instead of compacting them
;; (compaction stalls on macOS with many fonts installed).
(setq redisplay-skip-fontification-on-input t
      fast-but-imprecise-scrolling t
      inhibit-compacting-font-caches t
      auto-window-vscroll nil)

;; Bidirectional text analysis is expensive and never needed for code.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Smooth trackpad scrolling.
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(defconst my/cache-dir (expand-file-name "var/" user-emacs-directory)
  "Directory for cache-like state that should never be version controlled.")

(dolist (dir (list my/cache-dir
                   (expand-file-name "auto-save/" my/cache-dir)
                   (expand-file-name "backup/" my/cache-dir)))
  (make-directory dir t))

;; GUI Emacs on macOS does not inherit the shell PATH, so language servers and
;; formatters would be invisible without this.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Homebrew's standalone dvisvgm bundles its own kpathsea, which searches
;; relative to its own prefix and never finds the texlive formula's tree,
;; breaking Org LaTeX previews.  exec-path-from-shell only copies PATH, so
;; set the TEXMF variables here.  No-op when the directory is absent
;; (MacTeX needs none of this).
(when (and (not (getenv "TEXMFCNF"))
           (file-directory-p "/opt/homebrew/opt/texlive/share/texmf-dist/web2c"))
  (setenv "TEXMFCNF" "/opt/homebrew/opt/texlive/share/texmf-dist/web2c")
  (setenv "TEXMFROOT" "/opt/homebrew/opt/texlive/share")
  (setenv "TEXMFDIST" "/opt/homebrew/opt/texlive/share/texmf-dist"))

;; --- Editor defaults -----------------------------------------------------

(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 100)

(setq require-final-newline t
      sentence-end-double-space nil
      delete-by-moving-to-trash t
      create-lockfiles nil
      make-backup-files t
      version-control t
      kept-new-versions 10
      kept-old-versions 3
      delete-old-versions t
      backup-directory-alist `(("." . ,(expand-file-name "backup/" my/cache-dir)))
      auto-save-default t
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" my/cache-dir) t)))

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq ring-bell-function #'ignore
      use-short-answers t
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(column-number-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook #'visual-line-mode)

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Jump to any visible position from a couple of characters.
(use-package avy
  :bind ("M-j" . avy-goto-char-timer))

;; Trim trailing whitespace, but only on lines this session actually edited,
;; so shared files do not fill diffs with whitespace churn.
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; --- Session -------------------------------------------------------------

(setq recentf-save-file (expand-file-name "recentf" my/cache-dir)
      recentf-max-saved-items 200
      savehist-file (expand-file-name "history" my/cache-dir)
      history-length 200
      save-place-file (expand-file-name "places" my/cache-dir))

(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(use-package which-key
  :ensure nil ; built in since Emacs 30
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode 1))

(use-package editorconfig
  :ensure nil ; built in since Emacs 30
  :config (editorconfig-mode 1))

;; After `C-x o', a bare `o' keeps switching windows; likewise for undo,
;; window resizing, `M-g n'/`p' and every other command with a repeat map.
(use-package repeat
  :ensure nil
  :custom
  (repeat-exit-timeout 3)
  (repeat-exit-key "<escape>")
  :config (repeat-mode 1))

;; --- Dired ---------------------------------------------------------------

;; macOS ships BSD ls, which lacks --group-directories-first; GNU coreutils
;; installs it as `gls'.
(use-package dired
  :ensure nil
  :defer t ; `:config' below would otherwise load Dired (and Dirvish) at startup
  :custom
  (dired-dwim-target t)                ; copy/move targets the other Dired window
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (if (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-listing-switches "-AGFhlv --group-directories-first")
    (setq dired-listing-switches "-AFhlv")))

;; Dirvish takes over every Dired buffer: icons, file size and git state in
;; the listing, a preview pane, and `dirvish-side' as a project sidebar
;; (`C-c t s').  It draws the icons itself, so no nerd-icons-dired.
;; Pinned to MELPA: NonGNU's tarball leaves the extensions (dirvish-side,
;; dirvish-vc, ...) in a subdirectory that is not on `load-path'.
(use-package dirvish
  :pin melpa
  :after dired
  :custom
  (dirvish-attributes '(nerd-icons subtree-state vc-state file-size collapse))
  (dirvish-side-attributes '(nerd-icons subtree-state vc-state collapse))
  (dirvish-subtree-state-style 'nerd)
  (dirvish-use-header-line 'global)
  (dirvish-header-line-height 28)
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-side-width 34)
  (dirvish-side-follow-mode t)          ; sidebar tracks the visited file
  (dirvish-reuse-session t)
  :config
  (dirvish-override-dired-mode 1))

(defun my/sidebar-toggle ()
  "Show the project sidebar, or hide it if it is visible.
`dirvish-side' itself cycles open -> focus -> close; this is a plain toggle.
Use `my/sidebar-focus' (C-c t S / SPC O) to jump into an open sidebar."
  (interactive)
  (if-let* ((win (dirvish-side--session-visible-p)))
      ;; `dirvish-quit' reads the session from the *current buffer*, which
      ;; `with-selected-window' alone does not switch.
      (with-selected-window win
        (with-current-buffer (window-buffer win)
          (dirvish-quit)))
    (dirvish-side)))

(defun my/sidebar-focus ()
  "Select the sidebar window, opening it first if needed."
  (interactive)
  (dirvish-side))

;; --- Appearance ----------------------------------------------------------

(defun my/font-installed-p (family)
  "Return non-nil when FAMILY is available."
  (member family (font-family-list)))

(defconst my/nerd-font-installed-p
  (and (display-graphic-p)
       (seq-some (lambda (f) (string-match-p "Nerd Font" f)) (font-family-list)))
  "Whether any Nerd Font is present, for icon-dependent packages.")

;; Plain modus is pure white on pure black (#ffffff / #000000).  The `-tinted'
;; pair uses a warmer background and a distinctly different syntax palette,
;; not merely a recoloured background; swap these two values to try it.
;; `M-x modus-themes-select' previews every variant interactively.
(defvar my/light-theme 'modus-operandi "Theme used in light mode.")
(defvar my/dark-theme 'modus-vivendi-tinted "Theme used in dark mode.")

(defun my/load-theme (theme)
  "Load THEME, first disabling any currently enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm))

(defun my/theme-mode ()
  "Return the shared theme mode, the symbol `light' or `dark'.
Read from $XDG_STATE_HOME/theme-mode, the file `theme-mode\=' writes, so
Emacs, Neovim, fish, tmux, Ghostty and starship agree on one answer.
Defaults to dark when the file is missing or unreadable."
  (let* ((base (or (getenv "XDG_STATE_HOME") (expand-file-name "~/.local/state")))
         (file (expand-file-name "theme-mode" base)))
    (if (and (file-readable-p file)
             (string-prefix-p "light"
                              (with-temp-buffer
                                (insert-file-contents file)
                                (string-trim (buffer-string)))))
        'light
      'dark)))

(defun my/apply-theme-mode (&optional mode)
  "Load the theme for MODE, defaulting to the shared mode on disk.
MODE may be a symbol or a string, the latter because `theme-mode\=' calls
this over emacsclient when the mode changes while Emacs is running."
  (interactive)
  (let ((mode (cond ((stringp mode) (intern mode))
                    (mode mode)
                    (t (my/theme-mode)))))
    (my/load-theme (if (eq mode 'light) my/light-theme my/dark-theme))))

(defun my/toggle-theme ()
  "Flip between `my/light-theme' and `my/dark-theme'.
Applies the new theme here immediately -- so this still works with no
Emacs server running -- and then hands the mode to `theme-mode\=', which
turns Neovim, fish, tmux, Ghostty and starship over to match."
  (interactive)
  (let ((mode (if (eq (my/theme-mode) 'light) 'dark 'light)))
    (my/apply-theme-mode mode)
    (when (executable-find "theme-mode")
      (start-process "theme-mode" nil "theme-mode" (symbol-name mode)))
    (message "Theme: %s" (car custom-enabled-themes))))

;; Emacs bundles the modus theme *files*, but not `modus-themes.el' itself,
;; which is where the customization options and the newest palettes live.
(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-org-blocks 'gray-background
        ;; Scaled Org headings.
        modus-themes-headings '((1 . (1.3)) (2 . (1.2)) (3 . (1.1)) (t . (1.0)))
        ;; Borderless mode line and invisible fringe: flat, like a modern
        ;; editor; spacious-padding supplies the breathing room instead.
        modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (fringe unspecified)
          (bg-tab-bar bg-main)
          (bg-tab-current bg-active)
          (bg-tab-other bg-dim)
          (bg-line-number-active unspecified)
          (bg-line-number-inactive unspecified)))
  :config
  ;; Follow the shared mode, so an Emacs started while the desktop is light
  ;; comes up light.  `C-c t t' flips it, and everything else with it.
  (my/apply-theme-mode))

;; Padding around windows and a subtle, borderless mode line.
(use-package spacious-padding
  :if (display-graphic-p)
  :custom
  (spacious-padding-widths
   '(:internal-border-width 12 :header-line-width 4 :mode-line-width 4
     :tab-width 4 :right-divider-width 16 :scroll-bar-width 0 :fringe-width 8))
  (spacious-padding-subtle-mode-line t)
  :config (spacious-padding-mode 1))

;; One tab per workspace, named after its project.  Tabs hold window
;; layouts, not buffers, so `SPC b' is still how you move between files.
(defun my/tab-bar-tab-name ()
  "Name the tab after the current project, else the current buffer."
  (if-let* ((project (project-current nil)))
      (project-name project)
    (tab-bar-tab-name-current)))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show 1)                       ; hide when there is a single tab
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-tab-name-function #'my/tab-bar-tab-name)
  :config (tab-bar-mode 1))

;; Header line: project › directory › file › enclosing definition.
(use-package breadcrumb
  :hook ((prog-mode text-mode) . breadcrumb-local-mode))

;; Highlight the current line where it helps orientation, not in prose.
(dolist (hook '(prog-mode-hook dired-mode-hook tabulated-list-mode-hook
                magit-mode-hook compilation-mode-hook))
  (add-hook hook #'hl-line-mode))

;; Indentation guides, tree-sitter aware where a grammar is present.  The
;; NS build cannot draw stipples, so on macOS the guides are characters.
(use-package indent-bars
  :hook ((python-ts-mode yaml-ts-mode toml-ts-mode json-ts-mode
          c-ts-mode c++-ts-mode rust-ts-mode go-ts-mode
          js-ts-mode typescript-ts-mode tsx-ts-mode)
         . indent-bars-mode)
  :custom
  (indent-bars-prefer-character (eq system-type 'darwin))
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-color '(highlight :face-bg t :blend 0.25))
  (indent-bars-highlight-current-depth '(:blend 0.55))
  (indent-bars-width-frac 0.2))

;; TODO / FIXME / NOTE / HACK stand out in comments.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Ligatures for the operators these fonts draw specially (Iosevka Comfy,
;; Commit Mono, FiraCode and JetBrains Mono all support this set).
(use-package ligature
  :if (display-graphic-p)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("->" "->>" "-<" "<-" "<--" "<->" "=>" "==>" "<=>" "<==" "=>>" ">>=" "=<<"
     "==" "!=" "===" "!==" "<=" ">=" "<<" ">>" "<<<" ">>>" "::" ":::" "..."
     ".." "&&" "||" "|>" "<|" "<|>" "//" "/*" "*/" "++" "+++" "--" "__" "~>"
     "<~" "~~" "~=" "/=" "=~" "#{" "#(" "#_" "#?" "#[" ";;" ":=" "=:" "<>"))
  (global-ligature-mode 1))

;; Candidate monospace families, in preference order: preset name, family,
;; weight, and a height adjustment in tenths of a point (Iosevka is narrow
;; and reads small, so it gets a bump; FiraCode's Regular is heavy, so it
;; gets Light).  Only installed families become presets.
(defconst my/mono-candidates
  '((iosevka   "Iosevka Comfy"            regular 10)
    (commit    "CommitMono Nerd Font"     regular 0)
    (jetbrains "JetBrainsMono Nerd Font"  regular 0)
    (fira      "FiraCode Nerd Font"       light   0)
    (inconsolata "Inconsolata Nerd Font"  regular 5))
  "Monospace families to offer as fontaine presets.")

(defconst my/mono-families
  (seq-filter (lambda (c) (my/font-installed-p (nth 1 c))) my/mono-candidates)
  "The installed subset of `my/mono-candidates'.")

(defconst my/mono-family (nth 1 (car my/mono-families))
  "Preferred monospace family, for packages that need one outside fontaine.")

;; Prose face for org/markdown (`variable-pitch-mode' in notes.el).  Charter
;; ships with macOS; without any of these, prose stays monospace.
(defconst my/prose-family
  (seq-find #'my/font-installed-p '("Charter" "Georgia"))
  "Proportional family for `variable-pitch'.")

(defun my/fontaine-presets ()
  "Build fontaine presets: one per installed family, plus a large and a
presentation size of each (`iosevka', `iosevka-large', `iosevka-present').
`fixed-pitch' anchors to the same family so code blocks and tables in
mixed-font buffers match code buffers exactly."
  (let ((base (if (eq system-type 'darwin) 160 130))
        presets)
    (pcase-dolist (`(,name ,family ,weight ,adjust) my/mono-families)
      (dolist (size `((""         . ,(+ base adjust))
                      ("-large"   . ,(+ base adjust 35))
                      ("-present" . ,(+ base adjust 105))))
        (push `(,(intern (format "%s%s" name (car size)))
                :default-family ,family
                :default-weight ,weight
                :default-height ,(cdr size)
                :fixed-pitch-family ,family)
              presets)))
    (append (nreverse presets)
            `((t :bold-weight semibold
                 :fixed-pitch-height 1.0
                 :variable-pitch-family ,my/prose-family
                 :variable-pitch-height 1.05)))))

;; `C-c t f' switches family or size live; the last choice persists across
;; sessions.  The `t' entry holds the shared defaults.
(use-package fontaine
  :if (and (display-graphic-p) my/mono-families)
  :custom
  (fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" my/cache-dir))
  :config
  (setq fontaine-presets (my/fontaine-presets))
  (fontaine-mode 1)
  (let ((saved (fontaine-restore-latest-preset)))
    (fontaine-set-preset (if (assq saved fontaine-presets)
                             saved
                           (caar my/mono-families)))))

;; Icon glyphs live in the Unicode private-use areas.  Fonts like Iosevka
;; Comfy and Commit Mono have nothing there, and macOS fallback only covers
;; the older U+E000 block, so the Material Design range (U+F0000+) used by
;; nerd-icons and doom-modeline rendered as hex boxes.  Route both ranges to
;; the symbols-only Nerd Font whatever the main family is.
(defconst my/symbol-font
  (seq-find #'my/font-installed-p
            '("Symbols Nerd Font Mono" "JetBrainsMono Nerd Font" "FiraCode Nerd Font"))
  "Family that supplies Nerd Font icon glyphs.")

(when (and (display-graphic-p) my/symbol-font)
  (dolist (range '((#xe000 . #xf8ff) (#xf0000 . #xfffff)))
    (set-fontset-font t range my/symbol-font nil 'prepend)))

;; Briefly highlight the current line after jumps and window switches, so
;; the eye finds point without hunting.
(use-package pulsar
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 8)
  :config
  (dolist (fn '(avy-goto-char-timer other-window recenter-top-bottom
                flymake-goto-next-error flymake-goto-prev-error))
    (add-to-list 'pulsar-pulse-functions fn))
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
  (with-eval-after-load 'consult
    (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
    (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry))
  (pulsar-global-mode 1))

(use-package nerd-icons
  :if my/nerd-font-installed-p)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 4)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-icon my/nerd-font-installed-p)
  (doom-modeline-major-mode-icon my/nerd-font-installed-p)
  (doom-modeline-minor-modes nil)
  :init (doom-modeline-mode 1))

;; --- Health check --------------------------------------------------------

(defconst my/emacs-health-checks
  '((required "git"                      "Magit, Forge, and project detection")
    (required "rg"                       "Project search via consult")
    (required "node"                     "TypeScript and JavaScript toolchain")
    (optional "fd"                       "Faster file finding")
    (optional "uv"                       "Python environments and runner")
    (optional "direnv"                   "envrc: per-project environments")
    (optional "jupytext"                 "code-cells: .ipynb editing")
    (optional "latex"                    "Org LaTeX previews")
    (optional "dvisvgm"                  "Org LaTeX previews (SVG backend)")
    (optional "vtsls"                    "TypeScript / JavaScript LSP")
    (optional "gopls"                    "Go LSP")
    (optional "goimports"                "Go imports and format")
    (optional "haskell-language-server-wrapper" "Haskell LSP")
    (optional "ormolu"                   "Haskell format")
    (optional "lua-language-server"      "Lua LSP")
    (optional "stylua"                   "Lua format")
    (optional "bash-language-server"     "Shell LSP")
    (optional "shellcheck"               "Shell lint")
    (optional "shfmt"                    "Shell format")
    (optional "rust-analyzer"            "Rust LSP")
    (optional "ocamllsp"                 "OCaml LSP")
    (optional "clangd"                   "C/C++ LSP (Xcode CLT or llvm)")
    (optional "clang-format"             "C/C++ format")
    (optional "basedpyright-langserver"  "Python LSP")
    (optional "yaml-language-server"     "YAML LSP")
    (optional "docker-langserver"        "Dockerfile LSP")
    (optional "ruff"                     "Python lint and format")
    (optional "prettier"                 "Web, JSON, YAML, Markdown format")
    (optional "sql-formatter"            "SQL format")
    (optional "ocamlformat"              "OCaml format")
    (optional "utop"                     "OCaml REPL (ships utop.el)")
    (optional "lldb-dap"                 "dape: Rust / native debugging")
    (optional "gh"                       "Forge authentication")
    (optional "enchant-2"                "jinx spellchecker backend")
    (optional "racket"                   "racket-mode back end and REPL")
    (optional "sml"                      "SML/NJ REPL for sml-mode")
    (optional "millet-ls"                "Standard ML LSP")
    (optional "ipython"                  "Richer Python REPL (falls back to python3)")
    (optional "gls"                      "GNU ls: Dired directories-first listing"))
  "External tools this configuration expects, checked by `my/emacs-health-check'.
debugpy is deliberately absent: it is a Python module dape resolves through
the project environment, so install it per project with `uv add --dev debugpy'.")

(defun my/emacs-health-check ()
  "Report which expected external tools are available on PATH."
  (interactive)
  (with-current-buffer (get-buffer-create "*Emacs Health*")
    (let ((inhibit-read-only t)
          (missing-required 0))
      (erase-buffer)
      (insert (format "Emacs health check — %s\n\n" (system-name)))
      (pcase-dolist (`(,kind ,command ,description) my/emacs-health-checks)
        (let ((found (executable-find command)))
          (when (and (eq kind 'required) (not found))
            (cl-incf missing-required))
          (insert (format "[%-7s] %-26s %-9s %s\n"
                          (if found "ok" "MISSING")
                          command
                          (if (eq kind 'required) "required" "optional")
                          description))))
      (insert (format "\nGrammars: %s\n"
                      (mapconcat
                       (lambda (l)
                         (format "%s%s" l (if (treesit-language-available-p l) "" "(!)")))
                       '(typescript tsx javascript rust python c cpp cmake lua
                         bash yaml json toml dockerfile)
                       " ")))
      ;; vterm builds a native module against the system libvterm; without
      ;; its headers the build fails and vterm reports libvterm as missing.
      (insert (format "\nNative modules: vterm-module %s\n"
                      (if (locate-file "vterm-module" load-path
                                       (list module-file-suffix))
                          "built"
                        "MISSING (needs libvterm headers; see setup.sh)")))
      (insert "\nLocal overrides:\n")
      (dolist (file '("local-pre.el" "local-post.el" "custom.el"))
        (insert (format "  %-16s %s\n" file
                        (if (file-exists-p (expand-file-name file user-emacs-directory))
                            "present" "absent"))))
      (unless (zerop missing-required)
        (insert (format "\n%d required tool(s) missing.\n" missing-required)))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'core)
;;; core.el ends here
