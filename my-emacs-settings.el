(require 'uniquify)
(require 'etags-select)
(require 'color-theme)
(require 'ace-jump-mode)

(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(add-hook 'sgml-mode-hook 'zencoding-mode)
(setq create-lockfiles nil)

(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq shell-file-name "/bin/bash")
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(setq fill-column 85)
(setq initial-major-mode 'emacs-lisp-mode)
(setq browse-url-generic-program "google-chrome")
(setq initial-scratch-message nil)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

(global-auto-revert-mode 1)
(ido-hacks-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

(setq *textmate-gf-exclude* "(/|^)(\\.+[^/]+|tmp|log|classes|build|public)($|/)|(\\.xcodeproj|\\.nib|\\.framework|\\.app|\\.pbproj|\\.pbxproj|\\.xcode|\\.xcodeproj|\\.bundle|\\.pyc)(/|$)")

;; only turn off menus if not osx
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))

(if (featurep 'ns)
    (set-frame-font "Menlo-15")
  (set-frame-font "Menlo-11"))

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

(set-face-foreground 'vertical-border "#fcf6e3")
(set-face-background 'vertical-border "#fcf6e3")

(paren-activate) ; mic-paren

(setq default-indicate-empty-lines t)

(recentf-mode 1)
(setq recentf-max-saved-items 80
      recentf-exclude '("/tmp/" "/ssh:"))

(color-theme-solarized-light)

(provide 'my-emacs-settings)

