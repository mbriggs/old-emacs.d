(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.haml$" . haml-mode))

(add-hook 'haml-mode-hook 'flymake-haml-load)
(add-hook 'sass-mode-hook 'flymake-sass-load)


(provide 'init-haml)
