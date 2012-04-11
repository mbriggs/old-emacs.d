(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))
(add-hook 'nxml-mode-hook
          (lambda ()
            (rng-validate-mode 0))
          t)

(defun insert-rhtml-%= ()
  (interactive)
  (insert "<%=  %>")
  (backward-char 3))

(defun insert-rhtml-% ()
  (interactive)
  (insert "<%  %>")
  (backward-char 3))

(provide 'init-rhtml)
