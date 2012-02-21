(require 'autopair)
(autopair-global-mode)

(add-hook 'term-mode-hook
          (lambda ()
            (setq autopair-dont-activate t)))

(set-default 'autopair-dont-activate #'(lambda () (eq major-mode 'sldb-mode)))

(provide 'init-autopair)
