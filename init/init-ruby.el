(load-file "~/.emacs.d/elisp/vanilla-ruby-mode.el")
(load-library "ruby-mode")
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru" . ruby-mode))

(setq interpreter-mode-alist
      (cons '("ruby" . ruby-mode) interpreter-mode-alist))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")

(setq ruby-use-encoding-map nil)


; make :: symbol constituents
;(modify-syntax-entry ?: "_" ruby-mode-syntax-table)

(eval-after-load "ruby-mode"
  '(progn
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))

;; (defface ruby-def-face
;;   '((t (:foreground "#0a2832" :weight bold)))
;;    "My custom face for string delimiters")
;; (font-lock-add-keywords 'ruby-mode '(("\\<\\(def\\|class\\)\\>" . ruby-def-face)))

(add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)

(defun require-absolute-ruby-path ()
  (interactive)
  (insert (concat "require '" (railway-root) "'"))
  (backward-char 1))


(defun insert-hashrocket ()
  (interactive)
  (insert " => "))

(defalias 'ri 'yari)

; run the current buffer using Shift-F7
(add-hook 'ruby-mode-hook (lambda () (local-set-key [S-f7] 'ruby-compilation-this-buffer)))
; run the current test function using F8 key
(add-hook 'ruby-mode-hook (lambda () (local-set-key [f7] 'ruby-compilation-this-test)))

(add-hook 'ruby-mode-hook (lambda () (local-set-key [f6] 'recompile)))

(provide 'init-ruby)
