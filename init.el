(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(push "~/scripts" exec-path)

(load "~/.emacs.d/config/sources.el")       ; plugins and where to get them
(load "~/.emacs.d/config/plugins.el")       ; plugin configuration
(load "~/.emacs.d/config/functions.el")     ; random functions
(load "~/.emacs.d/config/keymaps.el")       ; key bindings
(load "~/.emacs.d/config/settings.el")      ; random settings
(load "~/.emacs.d/config/hex-colors.el")    ; color hex code backgrounds
(load "~/.emacs.d/config/mode-bindings.el") ; bind modes to file extensions
(load "~/.emacs.d/config/color-hacks.el")   ; hacks to themes