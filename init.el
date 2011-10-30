(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(push "~/scripts" exec-path)

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

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-samee "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(textmate-mode)

(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)

(setq linum-format "%3d ")
(global-linum-mode)

(color-theme-solarized-light)
