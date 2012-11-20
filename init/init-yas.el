(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(setq yas/trigger-key "M-k")
(setq yas/next-field-key "M-k")
(setq yas/prev-field-key "M-K")

(defvar was-running-ac)
(defun yas-disable-modes ()
  (when (auto-complete-mode)
    (auto-complete-mode -1)
    (setq was-running-ac t)))

(defun yas-enable-modes ()
  (when was-running-ac
    (auto-complete-mode 1)
    (setq was-running-ac nil)))

(add-hook 'yas-before-expand-snippet-hook 'yas-disable-modes)
(add-hook 'yas-after-exit-snippet-hook 'yas-enable-modes)

(yas/global-mode 1)

(provide 'init-yas)
