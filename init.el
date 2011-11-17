(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(push "~/scripts" exec-path)

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