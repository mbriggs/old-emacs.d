(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(setq yas/trigger-key "M-k")
(setq yas/next-field-key "M-k")
(setq yas/prev-field-key "M-K")
(require 'yasnippet)

(yas/global-mode 1)

(provide 'init-yas)
