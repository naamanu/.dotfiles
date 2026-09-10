;;; init.el --- Configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Modules live in elisp/ and load in dependency order.  keys.el is last so
;; every map it binds into (including evil's states) already exists.
;; package.el has activated every installed package before this file runs
;; (early-init.el turns on quickstart); use-package only installs what is
;; missing.

;;; Code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; `emacs --batch -l init.el' (the validation command) implies -q, which
;; skips early-init.el and package activation.  Catch up, so that check
;; loads the same packages a real session does.  early-init.el is the only
;; place `package-quickstart' is set, so it doubles as the marker.
(unless (bound-and-true-p package-quickstart)
  (load (expand-file-name "early-init" user-emacs-directory) nil 'nomessage)
  (package-activate-all))

;; Machine-local overrides that should not live in the dotfiles repo.
(let ((local-pre (expand-file-name "local-pre.el" user-emacs-directory)))
  (when (file-exists-p local-pre)
    (load local-pre nil 'nomessage)))

;; Customize's scribbles go to custom.el, which holds only
;; `package-selected-packages' (machine-local).  Loaded before the modules so
;; anything it does set is in place when they run, not overridden afterwards.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; use-package is built into Emacs since 29; no bootstrap needed.
(require 'use-package)
(setq use-package-always-ensure t)

;; --- Modules -------------------------------------------------------------

(require 'core)
(require 'completion)
(require 'dev)
(require 'langs)
(require 'notes)
(require 'vim)
(require 'keys)

;; Fonts, padding, ligatures and icons need a GUI frame to probe.  Under
;; `emacs --daemon' that frame arrives later, with the first emacsclient; the
;; hook runs the setup once and then removes itself (core.el).
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/setup-appearance)
  (my/setup-appearance))

(let ((local-post (expand-file-name "local-post.el" user-emacs-directory)))
  (when (file-exists-p local-post)
    (load local-post nil 'nomessage)))

(provide 'init)
;;; init.el ends here
