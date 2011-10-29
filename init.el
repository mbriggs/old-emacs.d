(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(load "~/.emacs.d/config/plugins.el")
(load "~/.emacs.d/config/commands.el")
(load "~/.emacs.d/config/keymaps.el")
(load "~/.emacs.d/config/settings.el")

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; erb support
(load "~/.emacs.d/el-get/nxhtml/autostart.el")
(setq
  nxhtml-global-minor-mode t
  mumamo-background-colors nil
  nxhtml-skip-welcome t
  indent-region-mode t
  rng-nxml-auto-validate-flag nil
  nxml-degraded t)

(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . eruby-nxhtml-mumamo-mode)) 

(evil-mode 1)
(setq-default evil-shift-width 2)

(textmate-mode)

(setq linum-format "%3d ")
(global-linum-mode)

(color-theme-solarized-light)
