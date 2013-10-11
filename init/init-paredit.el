(dolist (hook '(clojure-mode-hook
                nrepl-repl-mode
                emacs-lisp-mode-hook))
  (add-hook hook (lambda () (paredit-mode 1))))


(provide 'init-paredit)
