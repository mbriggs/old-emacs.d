(dolist (hook '(clojure-mode-hook
                emacs-lisp-mode-hook))
  (add-hook hook (lambda () (paredit-mode 1))))

(dolist (hook '(ruby-mode-hook
                js2-mode-hook
                rhtml-mode-hook))
  (add-hook hook 'paredit-nonlisp))

(defun paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(provide 'init-paredit)