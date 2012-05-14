;; -*- coding: utf-8 -*-
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(push "/usr/local/bin" exec-path)
(push (expand-file-name "~/scripts") exec-path)
(push "/usr/local/bin" exec-path)

(mapcar 'require
        '(init-pre-package-variables
          init-packages
          init-autopair
          init-rhtml
          init-crontab
          init-css
          init-diminish
          init-clojure
          init-secrets
          init-erc
          init-csv
          init-dired
          init-flymake
          init-haml
          init-ido
          init-magit
          init-expand-region
          init-maxframe
          init-org
          init-rinari
          init-linum
          init-evil
          init-ack
          init-twit
          init-rspec
          init-autocomplete
          init-ansi-color
          init-ruby
          init-sass
          init-markdown
          init-moustache
          init-yaml
          init-ruby-block
          init-ruby-end
          init-eproject
          init-js
          init-lua
          init-git-blame
          init-volatile-highlight
          init-whitespace
          init-slime

          my-emacs-settings
          my-defuns
          my-keymaps
          my-modeline

          hexcolors
          mingle

          server))

(unless (server-running-p)
  (server-start))

(setq custom-file "~/.emacs.d/my-custom-variables.el")
(load custom-file)

(when (eq system-type 'darwin)
  (setq-default mf-offset-x 58)
  (setq-default mf-offset-y 26)
  (setq-default mf-display-padding-width 57)
  (setq-default mf-display-padding-height 45))
(maybe-maximize-frame)
