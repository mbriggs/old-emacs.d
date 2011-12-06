(dolist (hook '(clojure-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook (lambda ()
                   (enclose-mode nil)
                   (paredit-mode 1))))


(provide 'init-paredit)
