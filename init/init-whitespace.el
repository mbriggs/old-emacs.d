
(defun hide-trailing-whitespace (mode-hook)
  (add-hook mode-hook (lambda ()
                        (set (make-local-variable 'trailing-whitespace) nil))))

(mapcar 'hide-trailing-whitespace
        '(comint-mode-hook
          ibuffer-mode-hook
          compilation-mode-hook
          shell-mode-hook
          eshell-mode-hook
          dired-mode-hook
          erc-mode-hook))

(provide 'init-whitespace)
