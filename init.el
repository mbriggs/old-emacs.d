(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq ansi-color-names-vector
      ["black" "#c60007" "#728a05" "#a57705" "#2075c7" "#c61b6e" "#259185" "white"])

(load "~/.emacs.d/config/sources.el")             ; plugins and where to get them
(load "~/.emacs.d/config/plugins.el")             ; plugin configuration
(load "~/.emacs.d/config/escreen.el")             ; tab replacement
(load "~/.emacs.d/config/projects.el")            ; project configs
(load "~/.emacs.d/config/functions.el")           ; random functions
(load "~/.emacs.d/config/keymaps.el")             ; key bindings
(load "~/.emacs.d/config/settings.el")            ; random settings
(load "~/.emacs.d/config/mode-bindings.el")       ; bind modes to file extensions
(load "~/.emacs.d/config/color-hacks.el")         ; hacks to themes
(load "~/.emacs.d/config/customize-variables.el") ; customize-set-variables

;; random elisp from various places
(load "~/.emacs.d/config/hex-colors.el")
(load "~/.emacs.d/config/popup-shell.el")

(push "/usr/local/bin" exec-path)
(push (expand-file-name "~/scripts") exec-path)










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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anything-header ((((class color)) (:background "#81908F" :foreground "#fcf4dc"))))
 '(erb-face ((((class color) (min-colors 88) (background light)) (:background "#fcf4dc"))))
 '(flymake-errline ((((class color)) (:underline "#c60007"))))
 '(flymake-warnline ((((class color)) (:underline "#2075c7"))))
 '(hl-paren-face ((t (:weight bold))) t)
 '(isearch ((((class color)) (:background "#a57705" :underline "#a57705" :bold t :foreground "#fcf4dc"))))
 '(js2-error-face ((((class color) (background light)) (:foreground "#c60007"))))
 '(js2-external-variable-face ((t (:foreground "#bd3612"))))
 '(js2-function-param-face ((t (:foreground "#728a05"))))
 '(lazy-highlight ((((class color)) (:background "#5859b7" :foreground "#fcf4dc"))))
 '(magit-branch ((((class color)) (:background "#81908F" :foreground "#FCF4DC"))))
 '(magit-diff-add ((((class color)) (:foreground "#728a05"))))
 '(magit-diff-del ((((class color)) (:foreground "#c60007"))))
 '(magit-section-title ((((class color)) (:background "#81908F" :foreground "#FCF4DC"))))
 '(rainbow-delimiters-depth-1-face ((((class color)) (:foreground "#c60007"))))
 '(rainbow-delimiters-depth-2-face ((((class color)) (:foreground "#a57705"))))
 '(rainbow-delimiters-depth-3-face ((((class color)) (:foreground "#bd3612"))))
 '(rainbow-delimiters-depth-4-face ((((class color)) (:foreground "#c61b6e"))))
 '(rainbow-delimiters-depth-5-face ((((class color)) (:foreground "#5859b7"))))
 '(rainbow-delimiters-depth-6-face ((((class color)) (:foreground "#2075c7"))))
 '(rainbow-delimiters-depth-7-face ((((class color)) (:foreground "#259185"))))
 '(rainbow-delimiters-depth-8-face ((((class color)) (:foreground "#728a05"))))
 '(rainbow-delimiters-depth-9-face ((((class color)) (:foreground "#042028"))))
 '(trailing-whitespace ((t (:background "#c7c0a9" :foreground "#e9e2cb" :inverse-video t)))))
