(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(push "~/scripts" exec-path)

(setq ansi-color-names-vector
      ["black" "#c60007" "#728a05" "#a57705" "#2075c7" "#c61b6e" "#259185" "white"])

(load "~/.emacs.d/config/sources.el")       ; plugins and where to get them
(load "~/.emacs.d/config/plugins.el")       ; plugin configuration
(load "~/.emacs.d/config/escreen.el")       ; tab replacement
(load "~/.emacs.d/config/functions.el")     ; random functions
(load "~/.emacs.d/config/keymaps.el")       ; key bindings
(load "~/.emacs.d/config/settings.el")      ; random settings
(load "~/.emacs.d/config/mode-bindings.el") ; bind modes to file extensions
(load "~/.emacs.d/config/color-hacks.el")   ; hacks to themes

;; random elisp from various places
(load "~/.emacs.d/config/hex-colors.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-always-indent-assigned-expr-in-decls-p t)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-consistent-level-indent-inner-bracket-p t)
 '(js2-enter-indents-newline t)
 '(js2-highlight-level 3)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(js2-strict-missing-semi-warning nil))