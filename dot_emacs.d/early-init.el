;;; early-init.el --- Pre-frame initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs before init.el, before package.el activates the installed packages
;; and before the first frame is drawn.  Keep it small: anything that does
;; not have to happen this early belongs in elisp/core.el.

;;; Code:

;; Raise the GC ceiling for startup; core.el restores a working value once the
;; session is up.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; package.el activates the installed packages itself, between this file and
;; init.el (`package-enable-at-startup', the default).  With quickstart on,
;; that activation loads one generated autoload file instead of scanning ~70
;; package descriptors; package.el regenerates the file after every install
;; or removal (`package-quickstart-refresh' by hand if it ever looks stale).
;; It is machine-local, never in chezmoi.  The archives are set here so they
;; are in place before package.el loads.
(setq package-quickstart t
      package-quickstart-file (expand-file-name "package-quickstart.el" user-emacs-directory)
      package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/"))
      ;; Prefer stable GNU/NonGNU where a package exists in both.
      package-archive-priorities '(("gnu" . 3) ("nongnu" . 2) ("melpa" . 1)))

;; A chezmoi-updated .el must never lose to a stale .elc sitting next to it.
(setq load-prefer-newer t)

;; Drop UI chrome before the first frame renders, to avoid a visible reflow.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Start maximized.  Deliberately no `width'/`height' here: pinning character
;; dimensions makes the window manager's maximize fight Emacs, which on macOS
;; leaves dead space at the frame edge instead of reflowing the text.
(push '(fullscreen . maximized) default-frame-alist)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      frame-inhibit-implied-resize t)

;; Only builds with native compilation have these: a stock Homebrew `emacs'
;; formula has none, the emacs-app cask does.
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t))

;;; early-init.el ends here
