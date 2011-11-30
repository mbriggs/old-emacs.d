(dolist (hook '(clojure-mode-hook
                js2-mode-hook
                elisp-mode-hook
                ruby-mode-hook))
  (add-hook hook (lambda () (paredit-mode 1))))
(provide 'init-paredit)