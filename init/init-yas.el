(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(setq yas/trigger-key "M-k")
(setq yas/next-field-key "M-k")
(setq yas/prev-field-key "M-K")

(defun yas-disable-modes ()
  (auto-complete-mode -1))

(defun yas-enable-modes ()
  (auto-complete-mode 1))

(add-hook 'yas-before-expand-snippet-hook 'yas-disable-modes)
(add-hook 'yas-after-exit-snippet-hook 'yas-enable-modes)

(yas/global-mode 1)

(provide 'init-yas)
