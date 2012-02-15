(require 'highlight-symbol)


(defun setup-highlight-symbol-mode-for (mode)
  (add-hook mode (lambda ()
                   (setq highlight-symbol-idle-delay 1)
                   (highlight-symbol-mode))))

(mapc 'setup-highlight-symbol-mode-for
      '(ruby-mode-hook
        js3-mode-hook
        js2-mode-hook
        emacs-lisp-mode-hook
        clojure-mode-hook
        sass-mode-hook
        css-mode-hook
        nxml-mode-hook))

(provide 'init-highlight-symbol)
