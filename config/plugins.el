(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode)) 

(evil-mode 1)
(evil-initial-state 'mo-git-blame 'emacs)
(evil-initial-state 'dired 'emacs)
(setq-default evil-shift-width 2)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'anything-project-files)

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

(require 'auto-complete-yasnippet)
(require 'tidy)
(require 'rinari)

(ruby-block-mode t)
(setq ruby-end-insert-newline nil)

(setq linum-format "%3d ")
(global-linum-mode)

(setq ibuffer-default-sorting-mode 'major-mode)

(setq show-trailing-whitespace t)

(setq yas/root-directory "~/.emacs.d/snippets/")
(yas/load-directory yas/root-directory)
(yas/global-mode)
(add-hook 'rinari-minor-mode-hook
          (lambda ()
            (setq yas/mode-symbol 'rails-mode)))