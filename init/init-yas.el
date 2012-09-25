(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(setq yas/trigger-key "M-k")
(setq yas/next-field-key "M-K")
(setq yas/prev-field-key "M-J")
(require 'yasnippet)

(yas/global-mode 1)

(provide 'init-yas)
