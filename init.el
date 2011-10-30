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

(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru" . ruby-mode))

(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode)) 

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

(require 'rinari)

(setq linum-format "%3d ")
(global-linum-mode)

(color-theme-solarized-light)