;; -*- coding: utf-8 -*-
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(push "/usr/local/bin" exec-path)
(push "/usr/bin" exec-path)
(push "/usr/local/share/npm/bin" exec-path)
(push (expand-file-name "~/scripts") exec-path)
(setenv "PATH" (concat "/usr/local/bin:" (expand-file-name "~/scripts") ":" (getenv "PATH")))

(cd "~")

(mapcar 'require
        '(init-pre-package-variables
          init-packages
          init-autopair
          init-web
          init-anzu
          init-crontab
          init-evil
          init-css
          init-diminish
          init-secrets
          init-erc
          init-python
          init-projectile
          init-csv
          init-dired
          init-flymake
          init-haml
          init-jade
          init-webjump
          init-ido
          init-magit
          init-expand-region
          init-maxframe
          init-org
          init-rinari
          init-linum
          init-ack
          init-serve-rails
          init-twit
          init-yas
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
          init-clojure
          init-js
          init-multiple-cursors
          init-railgun
          init-paredit
          init-lua
          init-git-blame
          init-volatile-highlight
          init-whitespace

          my-emacs-settings
          my-defuns
          my-keymaps
          my-ex-mode-maps
          my-modeline

          hexcolors
          mingle

          server))

(require 'test-js)
(require 'smartscan)
(require 'stylus-mode)
(require 'web-mode)

(unless (server-running-p)
  (server-start))

(setq custom-file "~/.emacs.d/my-custom-variables.el")
(load custom-file)
