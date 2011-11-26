(autoload 'markdown-mode "markdown-mode" nil t)
(autoload 'gfm-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.text$" . gfm-mode))

(provide 'init-markdown)
