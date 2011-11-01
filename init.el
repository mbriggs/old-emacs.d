(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(push "~/scripts" exec-path)

(load "~/.emacs.d/config/sources.el")
(load "~/.emacs.d/config/plugins.el")
(load "~/.emacs.d/config/functions.el")
(load "~/.emacs.d/config/keymaps.el")
(load "~/.emacs.d/config/settings.el")
(load "~/.emacs.d/config/hex-colors.el")

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru" . ruby-mode))

(setq ibuffer-default-sorting-mode 'major-mode)

(defface solarized-string-delimiter-face
  '((t (:foreground "#c60007" :weight bold)))
   "string delimiters being the start and end sigil")

(add-hook 'after-change-major-mode-hook 
          (lambda () 
            (font-lock-add-keywords nil '(("\\s\"\\|\\s|" 0 'solarized-string-delimiter-face t)))))

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(magit-branch ((((class color)) (:background "#81908F" :foreground "#FCF4DC"))))
 '(magit-section-title ((((class color)) (:background "#81908F" :foreground "#FCF4DC")))))
