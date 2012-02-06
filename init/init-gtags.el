(autoload 'gtags-mode "gtags" "" t)
(require 'popup-gtags)
(setq tb:gtags-use-elscreen nil)

(defun setup-global-mode-for (mode)
 (add-hook mode (lambda ()
		 (gtags-mode 1)
		 (setq gtags-symbol-regexp "[A-Za-z_:][A-Za-z0-9_#.:?]*"))))

(mapc 'setup-global-mode-for
      '(ruby-mode-hook
        js3-mode-hook
        js2-mode-hook))

(define-key ruby-mode-map "\e." 'gtags-find-tag)
(define-key ruby-mode-map "\e," 'gtags-find-with-grep)
(define-key ruby-mode-map "\e:" 'gtags-find-symbol)
(define-key ruby-mode-map "\e_" 'gtags-find-rtag)

(provide 'init-gtags)
