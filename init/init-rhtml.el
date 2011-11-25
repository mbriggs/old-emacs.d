(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))

(provide 'init-rhtml)